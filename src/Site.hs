{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Site
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
import           CompactThrift
import qualified Data.Attoparsec.ByteString.Lazy as ABL

------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Receive a message for a topic.
messageHandler :: AppHandler ()
messageHandler = do
      topic      <- (maybe (C.pack "no topic") id) <$> getParam "topic"
      req        <- getRequest
      body       <- readRequestBody 10485760
      result     <- liftIO $ (validateMessage body)
      source     <- return $ getHeader "X-Source-System" req
      uniqueId   <- return $ getHeader "X-Unique-Id" req
      if (Maybe.isJust (ABL.maybeResult result)) then goodMessage topic body else badMessage
  where
    goodMessage topic body = do
      result     <- send (C.unpack topic) (LBS.toStrict body)
      modifyResponse $ setResponseCode 200
    badMessage = do
      modifyResponse $ setResponseCode 400    

validateMessage str = do
  result  <- return $ ABL.parse parseCompactStruct str
  return $ result

send :: String -> ByteString -> AppHandler ()
send topic message = do
  topics <- gets _domains
  liftIO $ do
    message     <- return $ K.KafkaProduceMessage message
    kafkaTopic  <- return $ Maybe.fromJust (Map.lookup topic topics)
    maybeErr    <- K.produceMessage kafkaTopic K.KafkaUnassignedPartition message
    return ()

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
    t   <- liftIO $ (Map.singleton "foo") <$> (openKafkaTopic k "foo")
    addRoutes routes
    return $ App k t
