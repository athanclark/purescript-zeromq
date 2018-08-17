module ZeroMQ
  ( ZEROMQ, Pair, pair, Pub, pub, Sub, sub, XPub, xpub, XSub, xsub, Pull, pull
  , Push, push, Req, req, Rep, rep, Router, router, Dealer, dealer
  , Socket, class Connectable, socket
  , bind', unbind, bindSync, unbindSync, connect, disconnect
  , subscribe, unsubscribe, monitor, unmonitor
  , addMonitorListener, removeAllMonitorListeners
  , MonitorEvent, connectE, connectDelayE, connectRetryE, listenE, bindErrorE
  , acceptE, acceptErrorE, closeE, closeErrorE, disconnectE
  , Flag, sendMore, dontWait, close
  , send, sendMany, read, readSync
  ) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.NonEmpty (NonEmpty (..))
-- import Data.URI (Authority, URI (..), HierarchicalPart (..), Scheme (..))
-- import Data.URI.URI as URI
import Data.Array as Array
import Data.Time.Duration (Milliseconds)
import Data.Generic (class Generic)
import Data.Foreign (Foreign)
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


class Connectable from to where
  socket :: forall eff. from -> to -> Eff (zeromq :: ZEROMQ | eff) (Socket from to)

instance connectablePairPair :: Connectable Pair Pair where
  socket from _ = runEffFn1 socketImpl from

instance connectablePubSub :: Connectable Pub Sub where
  socket from _ = runEffFn1 socketImpl from

instance connectableSubPub :: Connectable Sub Pub where
  socket from _ = runEffFn1 socketImpl from

instance connectableXPubSub :: Connectable XPub Sub where
  socket from _ = runEffFn1 socketImpl from

instance connectableSubXPub :: Connectable Sub XPub where
  socket from _ = runEffFn1 socketImpl from

instance connectablePubXSub :: Connectable Pub XSub where
  socket from _ = runEffFn1 socketImpl from

instance connectableXSubPub :: Connectable XSub Pub where
  socket from _ = runEffFn1 socketImpl from

instance connectablePullPush :: Connectable Pull Push where
  socket from _ = runEffFn1 socketImpl from

instance connectablePushPull :: Connectable Push Pull where
  socket from _ = runEffFn1 socketImpl from

instance connectableRepReq :: Connectable Rep Req where
  socket from _ = runEffFn1 socketImpl from

instance connectableReqRep :: Connectable Req Rep where
  socket from _ = runEffFn1 socketImpl from

instance connectableRouterReq :: Connectable Router Req where
  socket from _ = runEffFn1 socketImpl from

instance connectableReqRouter :: Connectable Req Router where
  socket from _ = runEffFn1 socketImpl from

instance connectableRepDealer :: Connectable Rep Dealer where
  socket from _ = runEffFn1 socketImpl from

instance connectableDealerRep :: Connectable Dealer Rep where
  socket from _ = runEffFn1 socketImpl from

instance connectableDealerRouter :: Connectable Dealer Router where
  socket from _ = runEffFn1 socketImpl from

instance connectableRouterDealer :: Connectable Router Dealer where
  socket from _ = runEffFn1 socketImpl from


foreign import data Socket :: Type -> Type -> Type


foreign import socketImpl :: forall eff from to
                           . EffFn1 (zeromq :: ZEROMQ | eff) from (Socket from to)


foreign import bindImpl :: forall eff from to
                         . EffFn3 (zeromq :: ZEROMQ | eff)
                           (Socket from to)
                           String
                           (EffFn1 (zeromq :: ZEROMQ | eff) (Nullable Error) Unit)
                           Unit

bind' :: forall eff from to. Socket from to -> String -> Aff (zeromq :: ZEROMQ | eff) Unit
bind' s addr = makeAff \resolve -> do
  runEffFn3 bindImpl s addr $ mkEffFn1 \mE -> case toMaybe mE of
    Nothing -> resolve (Right unit)
    Just e -> resolve (Left e)
  pure nonCanceler


foreign import bindSyncImpl :: forall eff from to
                             . EffFn2 (zeromq :: ZEROMQ | eff)
                               (Socket from to)
                               String
                               Unit

bindSync :: forall eff from to. Socket from to -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
bindSync = runEffFn2 bindSyncImpl


foreign import unbindImpl :: forall eff from to
                           . EffFn3 (zeromq :: ZEROMQ | eff)
                             (Socket from to)
                             String
                             (EffFn1 (zeromq :: ZEROMQ | eff) (Nullable Error) Unit)
                             Unit

unbind :: forall eff from to. Socket from to -> String -> Aff (zeromq :: ZEROMQ | eff) Unit
unbind s addr = makeAff \resolve -> do
  runEffFn3 bindImpl s addr $ mkEffFn1 \mE -> case toMaybe mE of
    Nothing -> resolve (Right unit)
    Just e -> resolve (Left e)
  pure nonCanceler


foreign import unbindSyncImpl :: forall eff from to
                               . EffFn2 (zeromq :: ZEROMQ | eff)
                                 (Socket from to)
                                 String
                                 Unit

unbindSync :: forall eff from to. Socket from to -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
unbindSync = runEffFn2 unbindSyncImpl


foreign import connectImpl :: forall eff from to
                            . EffFn2 (zeromq :: ZEROMQ | eff)
                              (Socket from to)
                              String
                              Unit

connect :: forall eff from to. Socket from to -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
connect = runEffFn2 connectImpl


foreign import disconnectImpl :: forall eff from to
                               . EffFn2 (zeromq :: ZEROMQ | eff)
                                 (Socket from to)
                                 String
                                 Unit

disconnect :: forall eff from to. Socket from to -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
disconnect = runEffFn2 disconnectImpl


foreign import subscribeImpl :: forall eff from to
                              . EffFn2 (zeromq :: ZEROMQ | eff)
                                (Socket from to)
                                String
                                Unit

subscribe :: forall eff from to. Socket from to -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
subscribe = runEffFn2 subscribeImpl


foreign import unsubscribeImpl :: forall eff from to
                                . EffFn2 (zeromq :: ZEROMQ | eff)
                                  (Socket from to)
                                  String
                                  Unit

unsubscribe :: forall eff from to. Socket from to -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
unsubscribe = runEffFn2 unsubscribeImpl


foreign import data Flag :: Type

foreign import sendMore :: Flag

foreign import dontWait :: Flag


foreign import sendImpl :: forall eff from to
                         . EffFn3 (zeromq :: ZEROMQ | eff)
                           (Socket from to)
                           (Nullable Flag)
                           Buffer
                           Unit


send :: forall eff from to. Socket from to -> Maybe Flag -> Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit
send s f x = runEffFn3 sendImpl s (toNullable f) x


foreign import sendManyImpl :: forall eff from to
                             . EffFn2 (zeromq :: ZEROMQ | eff)
                               (Socket from to)
                               (Array Buffer)
                               Unit


sendMany :: forall eff from to. Socket from to -> NonEmpty Array Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit
sendMany s (NonEmpty x xs) = runEffFn2 sendManyImpl s ([x] <> xs)


foreign import readImpl :: forall eff from to
                         . EffFn1 (zeromq :: ZEROMQ | eff)
                           (Socket from to)
                           (Nullable (Array Buffer))

readSync :: forall eff from to. Socket from to -> Eff (zeromq :: ZEROMQ | eff) (Maybe (NonEmpty Array Buffer))
readSync s = do
  mxs <- runEffFn1 readImpl s
  case toMaybe mxs of
    Nothing -> pure Nothing
    Just xs -> case Array.uncons xs of
      Nothing -> pure Nothing
      Just {head,tail} -> pure $ Just $ NonEmpty head tail


newtype MonitorEvent = MonitorEvent String

derive instance genericMonitorEvent :: Generic MonitorEvent
derive newtype instance eqMonitorEvent :: Eq MonitorEvent

connectE :: MonitorEvent
connectE = MonitorEvent "connect"

connectDelayE :: MonitorEvent
connectDelayE = MonitorEvent "connect_delay"

connectRetryE :: MonitorEvent
connectRetryE = MonitorEvent "connect_retry"

listenE :: MonitorEvent
listenE = MonitorEvent "listen"

bindErrorE :: MonitorEvent
bindErrorE = MonitorEvent "bind_error"

acceptE :: MonitorEvent
acceptE = MonitorEvent "accept"

acceptErrorE :: MonitorEvent
acceptErrorE = MonitorEvent "accept_error"

closeE :: MonitorEvent
closeE = MonitorEvent "close"

closeErrorE :: MonitorEvent
closeErrorE = MonitorEvent "close_error"

disconnectE :: MonitorEvent
disconnectE = MonitorEvent "disconnect"


foreign import monitorImpl :: forall eff from to
                            . EffFn3 (zeromq :: ZEROMQ | eff)
                              (Socket from to)
                              Milliseconds
                              Int
                              Unit


monitor :: forall eff from to. Socket from to -> Milliseconds -> Int -> Eff (zeromq :: ZEROMQ | eff) Unit
monitor = runEffFn3 monitorImpl


foreign import unmonitorImpl :: forall eff from to
                              . EffFn1 (zeromq :: ZEROMQ | eff)
                                (Socket from to)
                                Unit

unmonitor :: forall eff from to. Socket from to -> Eff (zeromq :: ZEROMQ | eff) Unit
unmonitor = runEffFn1 unmonitorImpl


foreign import addMonitorListenerImpl :: forall eff from to
                                       . EffFn3 (zeromq :: ZEROMQ | eff)
                                         (Socket from to)
                                         MonitorEvent
                                         (EffFn1 (zeromq :: ZEROMQ | eff) Foreign Unit)
                                         Unit

addMonitorListener :: forall eff from to
                    . Socket from to
                   -> MonitorEvent
                   -> (Foreign -> Eff (zeromq :: ZEROMQ | eff) Unit)
                   -> Eff (zeromq :: ZEROMQ | eff) Unit
addMonitorListener s e f = runEffFn3 addMonitorListenerImpl s e (mkEffFn1 f)


foreign import removeAllMonitorListenersImpl :: forall eff from to
                                              . EffFn2 (zeromq :: ZEROMQ | eff)
                                                (Socket from to)
                                                MonitorEvent
                                                Unit

removeAllMonitorListeners :: forall eff from to. Socket from to -> MonitorEvent -> Eff (zeromq :: ZEROMQ | eff) Unit
removeAllMonitorListeners = runEffFn2 removeAllMonitorListenersImpl


foreign import closeImpl :: forall eff from to
                          . EffFn1 (zeromq :: ZEROMQ | eff)
                            (Socket from to)
                            Unit

close :: forall eff from to. Socket from to -> Eff (zeromq :: ZEROMQ | eff) Unit
close = runEffFn1 closeImpl


foreign import receiveImpl :: forall eff from to
                            . EffFn2 (zeromq :: ZEROMQ | eff)
                              (Socket from to)
                              (EffFn1 (zeromq :: ZEROMQ | eff) (Array Buffer) Unit)
                              Unit

read :: forall eff from to. Socket from to -> Aff (zeromq :: ZEROMQ | eff) (Array Buffer)
read s = makeAff \resolve -> do
  runEffFn2 receiveImpl s (mkEffFn1 \xs -> resolve (Right xs))
  pure nonCanceler
