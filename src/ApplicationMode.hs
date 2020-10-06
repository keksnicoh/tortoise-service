{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ApplicationMode
  ( ApplicationMode(..)
  )
where

data ApplicationMode
  = Development
  | Staging
  | Production
  deriving (Eq, Show)
