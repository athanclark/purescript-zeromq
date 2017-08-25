module Main where

import Prelude
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (delay, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow, errorShow)

import Data.Time.Duration (Milliseconds (..))
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.URI (Authority (..), Host (..), Port (..))
import ZeroMQ (registerProducer, registerWorker, registerPublisher, registerSubscriber)
import Node.Buffer (fromArray, toArray)


socket :: Authority
socket = Authority Nothing [Tuple (IPv4Address "127.0.0.1") (Just (Port 3000))]


producer :: Eff _ Unit
producer = do
  registerProducer socket $ \send -> do
    b <- fromArray [73,75,70]
    send b

worker :: Eff _ Unit
worker =
  registerWorker socket $ \message -> do
    a <- toArray message
    log $ show a

publisher :: Eff _ Unit
publisher =
  registerPublisher socket $ \send -> do
    b <- fromArray [73,75,70]
    void $ runAff errorShow (const $ pure unit) $ forever $ do
      delay $ Milliseconds 1000.0
      liftEff $ send "foo" b

subscriber :: Eff _ Unit
subscriber =
  registerSubscriber socket "foo" $ \message -> do
    a <- toArray message
    log $ show a
