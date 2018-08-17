module ZeroMQ
  ( ZEROMQ, Pair, pair, Pub, pub, Sub, sub, XPub, xpub, XSub, xsub, Pull, pull
  , Push, push, Req, req, Rep, rep, Router, router, Dealer, dealer
  , Socket, socket, bind
  ) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Nullable (Nullable, toMaybe)
-- import Data.URI (Authority, URI (..), HierarchicalPart (..), Scheme (..))
-- import Data.URI.URI as URI
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, runEffFn2, mkEffFn1, runEffFn1, runEffFn3)
import Control.Monad.Eff.Exception (Error)
import Node.Buffer (Buffer)


foreign import data ZEROMQ :: Effect

foreign import data ZMQ_Type :: Type

newtype Pair = Pair ZMQ_Type
foreign import pair :: Pair

newtype Pub = Pub ZMQ_Type
foreign import pub :: Pub

newtype Sub = Sub ZMQ_Type
foreign import sub :: Sub

newtype XPub = XPub ZMQ_Type
foreign import xpub :: XPub

newtype XSub = XSub ZMQ_Type
foreign import xsub :: XSub

newtype Pull = Pull ZMQ_Type
foreign import pull :: Pull

newtype Push = Push ZMQ_Type
foreign import push :: Push

newtype Req = Req ZMQ_Type
foreign import req :: Req

newtype Rep = Rep ZMQ_Type
foreign import rep :: Rep

newtype Router = Router ZMQ_Type
foreign import router :: Router

newtype Dealer = Dealer ZMQ_Type
foreign import dealer :: Dealer


foreign import data Socket :: Type -> Type


foreign import socketImpl :: forall eff from. EffFn1 (zeromq :: ZEROMQ | eff) from (Socket from)

socket :: forall eff from. from -> Eff (zeromq :: ZEROMQ | eff) (Socket from)
socket = runEffFn1 socketImpl


foreign import bindImpl :: forall eff from
                         . EffFn3 (zeromq :: ZEROMQ | eff)
                           (Socket from)
                           String
                           (EffFn1 (zeromq :: ZEROMQ | eff) (Nullable Error) Unit)
                           Unit

bind :: forall eff from. Socket from -> String -> Aff (zeromq :: ZEROMQ | eff) Unit
bind s addr = makeAff \resolve -> do
  runEffFn3 bindImpl s addr $ mkEffFn1 \mE -> case toMaybe mE of
    Nothing -> resolve (Right unit)
    Just e -> resolve (Left e)
  pure nonCanceler


foreign import bindSyncImpl :: forall eff from
                             . EffFn2 (zeromq :: ZEROMQ | eff)
                               (Socket from)
                               String
                               Unit

bindSync :: forall eff from. Socket from -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
bindSync = runEffFn2 bindSyncImpl


foreign import unbindImpl :: forall eff from
                           . EffFn3 (zeromq :: ZEROMQ | eff)
                             (Socket from)
                             String
                             (EffFn1 (zeromq :: ZEROMQ | eff) (Nullable Error) Unit)
                             Unit

unbind :: forall eff from. Socket from -> String -> Aff (zeromq :: ZEROMQ | eff) Unit
unbind s addr = makeAff \resolve -> do
  runEffFn3 bindImpl s addr $ mkEffFn1 \mE -> case toMaybe mE of
    Nothing -> resolve (Right unit)
    Just e -> resolve (Left e)
  pure nonCanceler


foreign import unbindSyncImpl :: forall eff from
                               . EffFn2 (zeromq :: ZEROMQ | eff)
                                 (Socket from)
                                 String
                                 Unit

unbindSync :: forall eff from. Socket from -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
unbindSync = runEffFn2 unbindSyncImpl


foreign import connectImpl :: forall eff from
                            . EffFn2 (zeromq :: ZEROMQ | eff)
                              (Socket from)
                              String
                              Unit

connect :: forall eff from. Socket from -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
connect = runEffFn2 connectImpl


foreign import disconnectImpl :: forall eff from
                               . EffFn2 (zeromq :: ZEROMQ | eff)
                                 (Socket from)
                                 String
                                 Unit

disconnect :: forall eff from. Socket from -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
disconnect = runEffFn2 disconnectImpl


foreign import subscribeImpl :: forall eff from
                              . EffFn2 (zeromq :: ZEROMQ | eff)
                                (Socket from)
                                String
                                Unit

subscribe :: forall eff from. Socket from -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
subscribe = runEffFn2 subscribeImpl


foreign import unsubscribeImpl :: forall eff from
                                . EffFn2 (zeromq :: ZEROMQ | eff)
                                  (Socket from)
                                  String
                                  Unit

unsubscribe :: forall eff from. Socket from -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
unsubscribe = runEffFn2 unsubscribeImpl


foreign import data Flag :: Type

foreign import sendMore :: Flag

foreign import dontWait :: Flag


foreign import sendImpl :: forall eff from
                         . EffFn3 (zeromq :: ZEROMQ | eff)
                           (Socket from)
                           (Nullable Flag)
                           Buffer
                           Unit


send :: forall eff from. Socket from -> Nullable Flag -> Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit
send = runEffFn3 sendImpl
