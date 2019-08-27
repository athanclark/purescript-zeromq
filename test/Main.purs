module Test.Main where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Traversable (traverse)
import Data.UUID (genUUID)
import Data.Time.Duration (Milliseconds (..))
import Node.Buffer (fromString, toString, fromArray)
import Node.Encoding (Encoding (UTF8))
import Data.Foldable (for_)
import Data.Enum (enumFromTo)
import Effect (Effect)
import Effect.Aff (runAff_, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throwException)

import ZeroMQ
  ( router, dealer, socket, bindSync, close, connect, disconnect, setOption, zmqIdentity
  , readMany, sendMany)



main :: Effect Unit
main = do
  runClient -- Server


runServer :: Effect Unit
runServer = do
  log "Running as Server"

  server <- socket dealer router
  let addr = "tcp://*:5561"
  bindSync server addr
  clientId <- (\x -> fromArray [0,0,0,0]) =<< genUUID
  setOption server zmqIdentity clientId

  hello <- fromString "Hello" UTF8

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> close server -- unbindSync server addr
  runAff_ resolve $ for_ (enumFromTo 1 10 :: Array Int) \i -> do
    liftEffect $ sendMany unit server $ NonEmpty hello []
    xs <- readMany server
    case xs of
      Nothing -> liftEffect $ log "no content?"
      Just {msg} -> do
        xs' <- liftEffect $ traverse (toString UTF8) msg
        liftEffect $ log $ show xs'
        delay $ Milliseconds $ 1000.0


runClient :: Effect Unit
runClient = do
  log "Running as Client"

  client <- socket router dealer
  let addr = "tcp://localhost:5561"
  connect client addr

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> disconnect client addr
  runAff_ resolve $ for_ (enumFromTo 1 10 :: Array Int) \_ -> do
    xs <- readMany client
    liftEffect $ case xs of
      Nothing -> log "no content?"
      Just {aux:addr',msg:NonEmpty y ys} -> do
        xs' <- traverse (toString UTF8) ([y] <> ys)
        log $ show xs'
        q <- fromString "?" UTF8
        sendMany addr' client $ NonEmpty y $ ys <> [q]
