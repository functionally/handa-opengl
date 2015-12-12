{-|
Module      :  Foreign.C.Types.Instances
Copyright   :  (c) 2015 Brian W Bush
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


import Data.Aeson (FromJSON)
import Data.Binary (Binary(..))
import Foreign.C.Types (CDouble(..), CFloat(..))
import GHC.Generics (Generic)


deriving instance Generic CFloat

instance Binary CFloat where
  put (CFloat x) = put x
  get = CFloat <$> get

instance FromJSON CFloat


deriving instance Generic CDouble

instance Binary CDouble where
  put (CDouble x) = put x
  get = CDouble <$> get

instance FromJSON CDouble
