{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Env
  ( ApplicationMode(..)
  )
where

data ApplicationMode
  = Development
  | Staging
  | Production
  deriving (Eq, Show)

