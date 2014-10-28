{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Bureaucracy.Application where

import Snap
------------------------------------------------------------------------------
import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Haskakafka as K

------------------------------------------------------------------------------
data App = App
    { _kafkaConnection  :: K.Kafka,
      _domains :: (Map.Map String K.KafkaTopic) }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App

