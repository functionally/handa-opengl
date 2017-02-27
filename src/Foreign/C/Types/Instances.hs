{-|
Module      :  $Header$
Copyright   :  (c) 2015-17 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Instances for C types.
-}


{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Foreign.C.Types.Instances (
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary(..))
import Foreign.C.Types (CDouble(..), CFloat(..))
import GHC.Generics (Generic)


deriving instance Generic CFloat

instance Binary CFloat where
  put (CFloat x) = put x
  get = CFloat <$> get

instance FromJSON CFloat

instance ToJSON CFloat


deriving instance Generic CDouble

instance Binary CDouble where
  put (CDouble x) = put x
  get = CDouble <$> get

instance FromJSON CDouble

instance ToJSON CDouble
