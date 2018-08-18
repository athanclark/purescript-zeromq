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
  runServer -- Client
  -- log "You should add some tests."


runServer = do
  server <- socket dealer router
  bindSync server "tcp://*:5561"
  clientId <- (\x -> fromArray [0,0,0,0]) =<< genUUID
  setOption server zmqIdentity clientId

  empty <- fromString "" UTF8
  hello <- fromString "Hello" UTF8

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> pure unit
  runAff_ resolve $ for_ (enumFromTo 1 10 :: Array Int) \i -> do
    liftEff $ sendMany server $ NonEmpty empty [hello]
    xs <- read server
    case Array.uncons xs of
      Nothing -> liftEff $ log "no content?"
      Just {head,tail} -> do
        xs' <- liftEff $ traverse (toString UTF8) tail
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
    liftEff $ case Array.uncons xs of
      Nothing -> log "no content?"
      Just {head,tail} -> do
        xs' <- traverse (toString UTF8) xs
        log $ show xs'
        q <- fromString "?" UTF8
        sendMany client $ NonEmpty head $ tail <> [q]
