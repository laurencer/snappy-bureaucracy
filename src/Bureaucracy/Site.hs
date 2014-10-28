{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Bureaucracy.Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Haskakafka as K
import           Snap.Core
import           Snap.Snaplet
import           Text.InterpolatedString.Perl6 (qc)

import           Bureaucracy.Thrift.CompactThrift

import qualified Data.Attoparsec.ByteString.Lazy as ABL


import Bureaucracy.Thrift.LanguageThrift ()

------------------------------------------------------------------------------
import           Bureaucracy.Application


------------------------------------------------------------------------------
-- | Receive a message for a topic.
messageHandler :: AppHandler ()
messageHandler = do
      topic      <- (maybe (C.pack "no topic") id) <$> getParam "topic"
      req        <- getRequest
      body       <- readRequestBody 10485760
      strictBody <- return $ LBS.toStrict body
      result     <- return $ validateMessage body
      source     <- return $ getHeader "X-Source-System" req
      uniqueId   <- return $ getHeader "X-Unique-Id" req
      if (Maybe.isJust (ABL.maybeResult result)) 
        then goodMessage topic strictBody 
        else badMessage topic strictBody
  where
    goodMessage topic body = do
      sendResult <- send (C.unpack topic) body
      maybe (modifyResponse $ setResponseCode 200) handleKafkaError sendResult
    badMessage topic body = do
      sendResult <- send [qc|{C.unpack topic}.bad|] body
      maybe (modifyResponse $ setResponseCode 400) handleKafkaError sendResult

-- | Ensures that the message is valid.
validateMessage bytes =
  ABL.parse parseCompactStruct bytes

-- | Generates a response where there is a KafkaError.
handleKafkaError :: K.KafkaError -> AppHandler ()
handleKafkaError err = do
  writeBS [qc|Could not write to Kafka Queue: {err}|]
  modifyResponse $ setResponseCode 500

-- | Sends a message to a Kafka queue synchronously.
send :: String -> ByteString -> AppHandler (Maybe K.KafkaError)
send topic message = do
  topics <- gets _domains
  liftIO $ do
    message     <- return $ K.KafkaProduceMessage message
    kafkaTopic  <- return $ Maybe.fromJust (Map.lookup topic topics)
    K.produceMessage kafkaTopic K.KafkaUnassignedPartition message

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/v1/:topic/message",    method POST messageHandler)
         ]

------------------------------------------------------------------------------
-- | The application initializer.


openKafkaProducer :: IO K.Kafka
openKafkaProducer = do
  config      <- K.newKafkaConf 
  kafka       <- K.newKafka K.KafkaProducer config
  _           <- K.addBrokers kafka "localhost:9092"
  return kafka

openKafkaTopic :: K.Kafka -> String -> IO K.KafkaTopic
openKafkaTopic kafka topic = do
  topicConfig <- K.newKafkaTopicConf
  K.newKafkaTopic kafka topic topicConfig

app :: SnapletInit App App
app = makeSnaplet "app" "Snappy Bureaucracy." Nothing $ do
    k   <- liftIO $ openKafkaProducer
    t   <- liftIO $ (Map.singleton "rdbms_changes") <$> (openKafkaTopic k "rdbms_changes")
    addRoutes routes
    return $ App k t
