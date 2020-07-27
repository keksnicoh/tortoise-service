{-# LANGUAGE DataKinds, PolyKinds, FlexibleInstances,
  ScopedTypeVariables, AllowAmbiguousTypes, FlexibleContexts,
  TypeApplications, RankNTypes, ConstraintKinds,
  UndecidableInstances, MultiParamTypeClasses #-}

{-|
Module      : Env

Provides Type Classes and convinience methods to interact with an environment
defined by HList.

-}
module OpenEnv
  ( nil
  , (#:)
  , HList
  , Provides(..)
  , ProvidesF(..)
  , EmbeddedF(..)
  , provide
  , provideF
  , embedded
  , embeddedF
  , Embedded
  , getValue
  )
where

import           Control.Monad.Reader           ( ReaderT(..)
                                                , asks
                                                , join
                                                , MonadReader
                                                )
import           Control.Monad.Identity         ( runIdentity
                                                , Identity
                                                )
import           HList

type Provides a e = ProvidesF Identity a e
type Embedded a e m = EmbeddedF Identity a e m

-- | provides an effectfull computation
class ProvidesF f a e where
  provideFromF :: e -> f a

-- | provides an embedded effectfull computation
class EmbeddedF f a e m where
  embeddedFromF :: e -> m (f a)

instance Get f t ts => ProvidesF f t (HList ts) where
  provideFromF = getF

instance (Traversable t, Applicative m, Get t (m a) ts)
  => EmbeddedF t a (HList ts) (ReaderT (HList ts) m) where
  embeddedFromF = ReaderT . const . sequenceA . getF

{-|
  returns a value from environment
  @
    getValue @String env
  @
-}
getValue :: forall t e . Provides t e => e -> t
getValue = runIdentity . provideFromF @Identity @t

{-|
  provides a value from environment
  @
    f :: (MonadReader e m, Provide String e) -> m String
    f = do
      string <- provide @String
      return string
  @
-}
provide :: forall t e m . (MonadReader e m, Provides t e) => m t
provide = asks $ runIdentity . provideFromF @Identity @t

{-|
  provides a boxed value from environment
  @
    f :: (MonadReader e m, ProvideF @[] String e) -> m [String]
    f = do
      strings <- provideF @String
      return strings
  @
-}
provideF :: forall f t e m . (MonadReader e m, ProvidesF f t e) => m (f t)
provideF = asks $ provideFromF @f @t

{-|
  performs an `m a` from environment within `MonadReader e m` by lifting it into ReaderT.

  @
    f :: (MonadReader e m, Embedded String e m) -> m String
    f = do
      string <- embedded @String
      return string
  @
-}
embedded :: forall a e m . (MonadReader e m, EmbeddedF Identity a e m) => m a
embedded = runIdentity <$> embeddedF @Identity @a

{-|
  receives an applicative effect within traversable context from environment

  @
    f :: (MonadReader e m, EmbeddedF [] String e m) -> m [String]
    f = do
      strings <- embeddedF @[] @String
      return strings
  @
-}
embeddedF :: forall t a e m . (MonadReader e m, EmbeddedF t a e m) => m (t a)
embeddedF = join . asks $ embeddedFromF
