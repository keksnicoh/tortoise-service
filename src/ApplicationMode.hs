{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ApplicationMode
  ( ApplicationMode(..)
  )
where

data ApplicationMode
  = Development
  | Staging
  | Production
  deriving (Eq, Show)
