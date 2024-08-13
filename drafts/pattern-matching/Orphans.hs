{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

module Orphans
    (
    ) where

import Data.Aeson   qualified as Aeson
import GHC.Generics (Generic)
import Lang

import Data.Kind    qualified as Hask

deriving via (r :: Hask.Type) instance (Aeson.ToJSON r) => (Aeson.ToJSON (TrivialEq r))
deriving via (r :: Hask.Type) instance (Aeson.ToJSON r) => (Aeson.ToJSON (TrivialOrd r))
deriving via (r :: Hask.Type) instance (Aeson.ToJSON r) => (Aeson.ToJSON (TrivialShow r))

deriving instance (Generic NameKind)
deriving instance (Generic Name)
deriving instance (Generic Literal)
deriving instance (Generic RawType)
deriving instance (Generic RawTermDef)
deriving instance (Generic RawPattern)
deriving instance (Generic Raw)
deriving instance (Generic RawDef)

instance Aeson.ToJSON NameKind where
instance Aeson.ToJSON Name where
instance Aeson.ToJSON Literal where
instance Aeson.ToJSON RawType where
instance Aeson.ToJSON RawTermDef where
instance Aeson.ToJSON RawPattern where
instance Aeson.ToJSON Raw where
instance Aeson.ToJSON RawDef where
