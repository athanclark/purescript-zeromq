module Test.Main where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Array as Array
import Data.NonEmpty (NonEmpty (..))
import Data.Traversable (traverse)
import Data.UUID (genUUID)
import Data.Time.Duration (Milliseconds (..))
import Node.Buffer (fromString, toString, fromArray)
import Node.Encoding (Encoding (UTF8))
import Data.Foldable (for_)
import Data.Enum (enumFromTo)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (runAff_, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (throwException)
import Unsafe.Coerce (unsafeCoerce)

import ZeroMQ
  ( ZEROMQ, router, dealer, socket, bindSync, connect, setOption, zmqIdentity
  , read, sendMany)



main :: Eff _ Unit
main = do
  runClient
  -- log "You should add some tests."


runServer = do
  server <- socket dealer router
  bindSync server "tcp://*:5561"
  clientId <- (\x -> fromArray [0,0,0,0]) =<< genUUID
  setOption server zmqIdentity clientId

  hello <- fromString "Hello" UTF8

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> pure unit
  runAff_ resolve $ for_ (enumFromTo 1 10 :: Array Int) \i -> do
    liftEff $ sendMany unit server $ NonEmpty hello []
    xs <- read server
    case xs of
      Nothing -> liftEff $ log "no content?"
      Just {msg} -> do
        xs' <- liftEff $ traverse (toString UTF8) msg
        liftEff $ log $ show xs'
        delay $ Milliseconds $ 1000.0


runClient = do
  client <- socket router dealer
  connect client "tcp://localhost:5561"

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> pure unit
  runAff_ resolve $ forever do
    xs <- read client
    liftEff $ case xs of
      Nothing -> log "no content?"
      Just {aux:addr,msg:NonEmpty y ys} -> do
        xs' <- traverse (toString UTF8) ([y] <> ys)
        log $ show xs'
        q <- fromString "?" UTF8
        sendMany addr client $ NonEmpty y $ ys <> [q]
