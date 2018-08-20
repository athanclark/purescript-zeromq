module ZeroMQ
  ( ZEROMQ, Pair, pair, Pub, pub, Sub, sub, XPub, xpub, XSub, xsub, Pull, pull
  , Push, push, Req, req, Rep, rep, Router, router, Dealer, dealer
  , Socket, class IsLegal, socket
  , ZMQ_Option, zmqIdentity, ZMQIdent (..)
  , setOption, getOption
  , bind', unbind, bindSync, unbindSync, class Bindable
  , connect, disconnect, class Connectable
  , subscribe, unsubscribe, class Subscriber
  , monitor, unmonitor
  , addMonitorListener, removeAllMonitorListeners
  , MonitorEvent, connectE, connectDelayE, connectRetryE, listenE, bindErrorE
  , acceptE, acceptErrorE, closeE, closeErrorE, disconnectE
  , Flag, sendMore, dontWait, close
  , class Sendable, sendMany, sendJson
  , class Receivable, readMany, readJson, addReceiveListener, removeAllReceiveListeners
  , proxy, kind Location, Bound, Connected
  ) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.NonEmpty (NonEmpty (..))
import Data.Array as Array
import Data.Time.Duration (Milliseconds)
import Data.Generic (class Generic)
import Data.Foreign (Foreign)
import Data.Argonaut
  (class EncodeJson, encodeJson, class DecodeJson, decodeJson, jsonParser)
import Node.Buffer (BUFFER, Buffer, fromArray)
import Node.Buffer (fromString, toString) as Buffer
import Node.Encoding (Encoding (UTF8)) as Buffer
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried
  (EffFn1, EffFn2, EffFn3, runEffFn2, mkEffFn1, runEffFn1, runEffFn3)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Exception (Error)


-- * Types

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


class IsLegal from to
instance isLegalPairPair :: IsLegal Pair Pair
instance isLegalPubSub :: IsLegal Pub Sub
instance isLegalSubPub :: IsLegal Sub Pub
instance isLegalXPubSub :: IsLegal XPub Sub
instance isLegalSubXPub :: IsLegal Sub XPub
instance isLegalPubXSub :: IsLegal Pub XSub
instance isLegalXSubPub :: IsLegal XSub Pub
instance isLegalPullPush :: IsLegal Pull Push
instance isLegalPushPull :: IsLegal Push Pull
instance isLegalRepReq :: IsLegal Rep Req
instance isLegalReqRep :: IsLegal Req Rep
instance isLegalRouterReq :: IsLegal Router Req
instance isLegalReqRouter :: IsLegal Req Router
instance isLegalRepDealer :: IsLegal Rep Dealer
instance isLegalDealerRep :: IsLegal Dealer Rep
instance isLegalDealerRouter :: IsLegal Dealer Router
instance isLegalRouterDealer :: IsLegal Router Dealer


foreign import kind Location
foreign import data Bound :: Location
foreign import data Connected :: Location


class Bindable from
instance bindablePub :: Bindable Pub
instance bindableXPub :: Bindable XPub
instance bindableXSub :: Bindable XSub
instance bindableRep :: Bindable Rep
instance bindablePair :: Bindable Pair
instance bindablePull :: Bindable Pull
instance bindablePush :: Bindable Push
instance bindableRouter :: Bindable Router
instance bindableDealer :: Bindable Dealer


class Connectable from
instance connectablePair :: Connectable Pair
instance connectablePub :: Connectable Pub
instance connectableSub :: Connectable Sub
instance connectableXPub :: Connectable XPub
instance connectableXSub :: Connectable XSub
instance connectablePull :: Connectable Pull
instance connectablePush :: Connectable Push
instance connectableReq :: Connectable Req
instance connectableRep :: Connectable Rep
instance connectableRouter :: Connectable Router
instance connectableDealer :: Connectable Dealer


class Subscriber from
instance subscriberSub :: Subscriber Sub


class Sendable from to aux | from to -> aux where
  sendMany :: forall eff loc
            . aux
           -> Socket from to loc
           -> NonEmpty Array Buffer
           -> Eff (zeromq :: ZEROMQ | eff) Unit

sendJson :: forall from to aux a eff loc
          . Sendable from to aux
         => EncodeJson a
         => aux
         -> Socket from to loc
         -> a
         -> Eff (buffer :: BUFFER, zeromq :: ZEROMQ | eff) Unit
sendJson a s x = do
  buf <- Buffer.fromString (show $ encodeJson x) Buffer.UTF8
  sendMany a s (NonEmpty buf [])

instance sendablePubSub :: Sendable Pub Sub Unit where
  sendMany _ s xs = sendMany' s xs

instance sendableXPubSub :: Sendable XPub Sub Unit where
  sendMany _ s xs = sendMany' s xs

instance sendablePubXSub :: Sendable Pub XSub Unit where
  sendMany _ s xs = sendMany' s xs

instance sendableReqRep :: Sendable Req Rep Unit where
  sendMany _ s xs = sendMany' s xs

instance sendableRepReq :: Sendable Rep Req Unit where
  sendMany _ s xs = sendMany' s xs

instance sendableReqRouter :: Sendable Req Router Unit where
  sendMany _ s xs = sendMany' s xs

instance sendableRouterReq :: Sendable Router Req ZMQIdent where
  sendMany (ZMQIdent addr) s (NonEmpty x xs) = do
    empty <- unsafeCoerceEff (fromArray [])
    sendMany' s $ NonEmpty addr $ [empty, x] <> xs

instance sendableRepDealer :: Sendable Rep Dealer Unit where
  sendMany _ s xs = sendMany' s xs

instance sendableDealerRep :: Sendable Dealer Rep Unit where
  sendMany _ s (NonEmpty x xs) = do
    empty <- unsafeCoerceEff (fromArray [])
    sendMany' s $ NonEmpty empty $ [x] <> xs

instance sendableDealerRouter :: Sendable Dealer Router Unit where
  sendMany _ s (NonEmpty x xs) = do
    empty <- unsafeCoerceEff (fromArray [])
    sendMany' s $ NonEmpty empty $ [x] <> xs

instance sendableRouterDealer :: Sendable Router Dealer ZMQIdent where
  sendMany (ZMQIdent addr) s (NonEmpty x xs) = do
    empty <- unsafeCoerceEff (fromArray [])
    sendMany' s $ NonEmpty addr $ [empty, x] <> xs


class Receivable from to aux | from to -> aux where
  readMany :: forall eff loc
            . Socket from to loc
          -> Aff (zeromq :: ZEROMQ | eff)
              (Maybe { aux :: aux, msg :: NonEmpty Array Buffer })

readJson :: forall from to aux eff loc a
          . Receivable from to aux
         => DecodeJson a
         => Socket from to loc
         -> Aff (buffer :: BUFFER, zeromq :: ZEROMQ | eff)
            (Maybe { aux :: aux, msg :: a })
readJson s = do
  mX <- readMany s
  case mX of
    Nothing -> pure Nothing
    Just {aux,msg: NonEmpty msg _} -> do
      str <- liftEff (Buffer.toString Buffer.UTF8 msg)
      case decodeJson =<< jsonParser str of
        Left _ -> pure Nothing
        Right x -> pure (Just {aux,msg:x})

instance receivableSubPub :: Receivable Sub Pub Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
      _ -> pure Nothing

instance receivableSubXPub :: Receivable Sub XPub Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
      _ -> pure Nothing

instance receivableXSubPub :: Receivable XSub Pub Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
      _ -> pure Nothing

instance receivableReqRep :: Receivable Req Rep Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
      _ -> pure Nothing

instance receivableRepReq :: Receivable Rep Req Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
      _ -> pure Nothing

instance receivableReqRouter :: Receivable Req Router Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
      _ -> pure Nothing

instance receivableRouterReq :: Receivable Router Req ZMQIdent where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head:addr,tail:t1} -> case Array.uncons t1 of
        Just {head:_,tail:t2} -> case Array.uncons t2 of
          Just {head,tail} ->
            pure $ Just {aux: ZMQIdent addr, msg: NonEmpty head tail}
          _ -> pure Nothing
        _ -> pure Nothing
      _ -> pure Nothing

instance receivableRepDealer :: Receivable Rep Dealer Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
      _ -> pure Nothing

instance receivableDealerRep :: Receivable Dealer Rep Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head:_,tail:t1} -> case Array.uncons t1 of
        Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
        _ -> pure Nothing
      _ -> pure Nothing

instance receivableDealerRouter :: Receivable Dealer Router Unit where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head:_,tail:t1} -> case Array.uncons t1 of
        Just {head,tail} -> pure $ Just {aux: unit, msg: NonEmpty head tail}
        _ -> pure Nothing
      _ -> pure Nothing

instance receivableRouterDealer :: Receivable Router Dealer ZMQIdent where
  readMany s = do
    xs <- read' s
    case Array.uncons xs of
      Just {head:addr,tail:t1} -> case Array.uncons t1 of
        Just {head:_,tail:t2} -> case Array.uncons t2 of
          Just {head,tail} ->
            pure $ Just {aux: ZMQIdent addr, msg: NonEmpty head tail}
          _ -> pure Nothing
        _ -> pure Nothing
      _ -> pure Nothing



newtype Flag = Flag Int

derive instance genericFlag :: Generic Flag
derive newtype instance eqFlag :: Eq Flag

foreign import sendMore :: Flag
foreign import dontWait :: Flag


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


newtype ZMQ_Option = ZMQ_Option Int
derive instance genericZMQOption :: Generic ZMQ_Option
derive newtype instance eqZMQOption :: Eq ZMQ_Option


foreign import zmqIdentity :: ZMQ_Option



foreign import data Socket :: Type -> Type -> Location -> Type


newtype ZMQIdent = ZMQIdent Buffer
-- derive instance genericZMQIdent :: Generic ZMQIdent
-- derive newtype instance eqZMQIdent :: Eq ZMQIdent


-- * Functions

foreign import socketImpl :: forall eff from to loc
                           . EffFn1 (zeromq :: ZEROMQ | eff) from (Socket from to loc)

socket :: forall from to loc eff
        . IsLegal from to
       => from -> to -> Eff (zeromq :: ZEROMQ | eff) (Socket from to loc)
socket from _ = runEffFn1 socketImpl from


foreign import setOptionImpl :: forall eff from to loc
                              . EffFn3 (zeromq :: ZEROMQ | eff)
                                (Socket from to loc)
                                ZMQ_Option
                                Buffer
                                Unit

setOption :: forall eff from to loc
           . Socket from to loc -> ZMQ_Option -> Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit
setOption = runEffFn3 setOptionImpl


foreign import getOptionImpl :: forall eff from to loc
                              . EffFn2 (zeromq :: ZEROMQ | eff)
                                (Socket from to loc)
                                ZMQ_Option
                                (Nullable Buffer)

getOption :: forall eff from to loc
           . Socket from to loc -> ZMQ_Option -> Eff (zeromq :: ZEROMQ | eff) (Maybe Buffer)
getOption s o = toMaybe <$> runEffFn2 getOptionImpl s o


foreign import bindImpl :: forall eff from to
                         . EffFn3 (zeromq :: ZEROMQ | eff)
                           (Socket from to Bound)
                           String
                           (EffFn1 (zeromq :: ZEROMQ | eff) (Nullable Error) Unit)
                           Unit

bind' :: forall eff from to
       . Bindable from
      => Socket from to Bound -> String -> Aff (zeromq :: ZEROMQ | eff) Unit
bind' s addr = makeAff \resolve -> do
  runEffFn3 bindImpl s addr $ mkEffFn1 \mE -> case toMaybe mE of
    Nothing -> resolve (Right unit)
    Just e -> resolve (Left e)
  pure nonCanceler


foreign import bindSyncImpl :: forall eff from to
                             . EffFn2 (zeromq :: ZEROMQ | eff)
                               (Socket from to Bound)
                               String
                               Unit

bindSync :: forall eff from to. Bindable from => Socket from to Bound -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
bindSync = runEffFn2 bindSyncImpl


foreign import unbindImpl :: forall eff from to
                           . EffFn3 (zeromq :: ZEROMQ | eff)
                             (Socket from to Bound)
                             String
                             (EffFn1 (zeromq :: ZEROMQ | eff) (Nullable Error) Unit)
                             Unit

unbind :: forall eff from to. Bindable from => Socket from to Bound -> String -> Aff (zeromq :: ZEROMQ | eff) Unit
unbind s addr = makeAff \resolve -> do
  runEffFn3 bindImpl s addr $ mkEffFn1 \mE -> case toMaybe mE of
    Nothing -> resolve (Right unit)
    Just e -> resolve (Left e)
  pure nonCanceler


foreign import unbindSyncImpl :: forall eff from to
                               . EffFn2 (zeromq :: ZEROMQ | eff)
                                 (Socket from to Bound)
                                 String
                                 Unit

unbindSync :: forall eff from to. Bindable from => Socket from to Bound -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
unbindSync = runEffFn2 unbindSyncImpl


foreign import connectImpl :: forall eff from to
                            . EffFn2 (zeromq :: ZEROMQ | eff)
                              (Socket from to Connected)
                              String
                              Unit

connect :: forall eff from to. Connectable from => Socket from to Connected -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
connect = runEffFn2 connectImpl


foreign import disconnectImpl :: forall eff from to
                               . EffFn2 (zeromq :: ZEROMQ | eff)
                                 (Socket from to Connected)
                                 String
                                 Unit

disconnect :: forall eff from to. Connectable from => Socket from to Connected -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
disconnect = runEffFn2 disconnectImpl


foreign import subscribeImpl :: forall eff from to loc
                              . EffFn2 (zeromq :: ZEROMQ | eff)
                                (Socket from to loc)
                                String
                                Unit

subscribe :: forall eff from to loc. Subscriber from => Socket from to loc -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
subscribe = runEffFn2 subscribeImpl


foreign import unsubscribeImpl :: forall eff from to loc
                                . EffFn2 (zeromq :: ZEROMQ | eff)
                                  (Socket from to loc)
                                  String
                                  Unit

unsubscribe :: forall eff from to loc. Subscriber from => Socket from to loc -> String -> Eff (zeromq :: ZEROMQ | eff) Unit
unsubscribe = runEffFn2 unsubscribeImpl


-- foreign import sendImpl :: forall eff from to loc
--                          . EffFn3 (zeromq :: ZEROMQ | eff)
--                            (Socket from to loc)
--                            (Nullable Flag)
--                            Buffer
--                            Unit


-- send :: forall eff from to loc. Socket from to loc -> Maybe Flag -> Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit
-- send s f x = runEffFn3 sendImpl s (toNullable f) x


foreign import sendManyImpl :: forall eff from to loc
                             . EffFn2 (zeromq :: ZEROMQ | eff)
                               (Socket from to loc)
                               (Array Buffer)
                               Unit


sendMany' :: forall eff from to loc
           . Socket from to loc
          -> NonEmpty Array Buffer
          -> Eff (zeromq :: ZEROMQ | eff) Unit
sendMany' s (NonEmpty x xs) = runEffFn2 sendManyImpl s ([x] <> xs)


-- foreign import readImpl :: forall eff from to loc
--                          . EffFn1 (zeromq :: ZEROMQ | eff)
--                            (Socket from to loc)
--                            (Nullable (Array Buffer))

-- readSync :: forall eff from to loc. Socket from to loc -> Eff (zeromq :: ZEROMQ | eff) (Maybe (NonEmpty Array Buffer))
-- readSync s = do
--   mxs <- runEffFn1 readImpl s
--   case toMaybe mxs of
--     Nothing -> pure Nothing
--     Just xs -> case Array.uncons xs of
--       Nothing -> pure Nothing
--       Just {head,tail} -> pure $ Just $ NonEmpty head tail


foreign import monitorImpl :: forall eff from to loc
                            . EffFn3 (zeromq :: ZEROMQ | eff)
                              (Socket from to loc)
                              Milliseconds
                              Int
                              Unit


monitor :: forall eff from to loc. Socket from to loc -> Milliseconds -> Int -> Eff (zeromq :: ZEROMQ | eff) Unit
monitor = runEffFn3 monitorImpl


foreign import unmonitorImpl :: forall eff from to loc
                              . EffFn1 (zeromq :: ZEROMQ | eff)
                                (Socket from to loc)
                                Unit

unmonitor :: forall eff from to loc. Socket from to loc -> Eff (zeromq :: ZEROMQ | eff) Unit
unmonitor = runEffFn1 unmonitorImpl


foreign import addMonitorListenerImpl :: forall eff from to loc
                                       . EffFn3 (zeromq :: ZEROMQ | eff)
                                         (Socket from to loc)
                                         MonitorEvent
                                         (EffFn1 (zeromq :: ZEROMQ | eff) Foreign Unit)
                                         Unit

addMonitorListener :: forall eff from to loc
                    . Socket from to loc
                   -> MonitorEvent
                   -> (Foreign -> Eff (zeromq :: ZEROMQ | eff) Unit)
                   -> Eff (zeromq :: ZEROMQ | eff) Unit
addMonitorListener s e f = runEffFn3 addMonitorListenerImpl s e (mkEffFn1 f)


foreign import removeAllMonitorListenersImpl :: forall eff from to loc
                                              . EffFn2 (zeromq :: ZEROMQ | eff)
                                                (Socket from to loc)
                                                MonitorEvent
                                                Unit

removeAllMonitorListeners :: forall eff from to loc. Socket from to loc -> MonitorEvent -> Eff (zeromq :: ZEROMQ | eff) Unit
removeAllMonitorListeners = runEffFn2 removeAllMonitorListenersImpl


foreign import closeImpl :: forall eff from to loc
                          . EffFn1 (zeromq :: ZEROMQ | eff)
                            (Socket from to loc)
                            Unit

close :: forall eff from to loc. Socket from to loc -> Eff (zeromq :: ZEROMQ | eff) Unit
close = runEffFn1 closeImpl


foreign import receiveImpl :: forall eff from to loc
                            . EffFn2 (zeromq :: ZEROMQ | eff)
                              (Socket from to loc)
                              (EffFn1 (zeromq :: ZEROMQ | eff) (Array Buffer) Unit)
                              Unit

read' :: forall eff from to loc
       . Socket from to loc
      -> Aff (zeromq :: ZEROMQ | eff) (Array Buffer)
read' s = makeAff \resolve -> do
  runEffFn2 receiveImpl s (mkEffFn1 \xs -> resolve (Right xs))
  pure nonCanceler


foreign import addReceiveListenerImpl :: forall eff from to loc
                                       . EffFn2 (zeromq :: ZEROMQ | eff)
                                         (Socket from to loc)
                                         (EffFn1 (zeromq :: ZEROMQ | eff) (Array Buffer) Unit)
                                         Unit

addReceiveListener :: forall eff from to loc
                    . Socket from to loc
                   -> (Array Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit)
                   -> Eff (zeromq :: ZEROMQ | eff) Unit
addReceiveListener s f = runEffFn2 addReceiveListenerImpl s (mkEffFn1 f)


foreign import removeAllReceiveListenersImpl :: forall eff from to loc
                                              . EffFn1 (zeromq :: ZEROMQ | eff)
                                                (Socket from to loc)
                                                Unit

removeAllReceiveListeners :: forall eff from to loc. Socket from to loc -> Eff (zeromq :: ZEROMQ | eff) Unit
removeAllReceiveListeners = runEffFn1 removeAllReceiveListenersImpl


foreign import proxyImpl :: forall eff from1 to1 from2 to2 from3 to3 loc3
                          . EffFn3 (zeromq :: ZEROMQ | eff)
                            (Socket from1 to1 Bound)
                            (Socket from2 to2 Bound)
                            (Nullable (Socket from3 to3 loc3))
                            Unit

proxy :: forall eff from1 to1 from2 to2 from3 to3 loc3
       . Socket from1 to1 Bound -- ^ Frontend
      -> Socket from2 to2 Bound -- ^ Backend
      -> Maybe (Socket from3 to3 loc3)
      -> Eff (zeromq :: ZEROMQ | eff) Unit
proxy f b c = runEffFn3 proxyImpl f b (toNullable c)
