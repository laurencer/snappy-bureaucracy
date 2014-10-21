{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Haskakafka as K
import           Snap.Core
import           Snap.Snaplet
import           Text.InterpolatedString.Perl6 (qc)
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Receive a message for a topic.
messageHandler :: AppHandler ()
messageHandler = do
      topic      <- (maybe (C.pack "no topic") id) <$> getParam "topic"
      req        <- getRequest
      source     <- return $ getHeader "X-Source-System" req
      uniqueId   <- return $ getHeader "X-Unique-Id" req
      result     <- send (C.unpack topic)
      writeBS [qc|The source is {source} and the uniqueId is {uniqueId}. Sent message to {topic}: {result}|]

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/:topic/message",    method POST messageHandler)
         ]

------------------------------------------------------------------------------
-- | The application initializer.


send :: String -> AppHandler ()
send topic = do
  topics <- gets _topics
  liftIO $ do
    message     <- return $ K.KafkaProduceMessage (C.pack "test")
    kafkaTopic  <- return $ Maybe.fromJust (Map.lookup topic topics)
    maybeErr    <- K.produceMessage kafkaTopic K.KafkaUnassignedPartition message
    return ()


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
    t   <- liftIO $ (Map.singleton "foo") <$> (openKafkaTopic k "foo")
    addRoutes routes
    return $ App k t
