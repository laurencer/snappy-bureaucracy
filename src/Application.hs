{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

import Snap
------------------------------------------------------------------------------
import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Haskakafka as K

------------------------------------------------------------------------------
data App = App
    { _kafka  :: K.Kafka,
      _topics :: (Map.Map String K.KafkaTopic) }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App

