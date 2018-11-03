module ZeroMQ
  ( Pair, pair, Pub, pub, Sub, sub, XPub, xpub, XSub, xsub, Pull, pull
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
  , class Receivable, readMany, readJson, readJson', addReceiveListener, removeAllReceiveListeners
  , proxy, kind Location, Bound, Connected
  ) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.NonEmpty (NonEmpty (..))
import Data.Array as Array
import Data.Time.Duration (Milliseconds)
import Data.Generic.Rep (class Generic)
import Foreign (Foreign)
import Data.Argonaut
  (class EncodeJson, encodeJson, class DecodeJson, decodeJson, jsonParser)
import Data.Argonaut as Argonaut
import Node.Buffer (Buffer, fromArray)
import Node.Buffer (fromString, toString) as Buffer
import Node.Encoding (Encoding (UTF8)) as Buffer
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Uncurried
   (EffectFn1, EffectFn2, EffectFn3, runEffectFn2, mkEffectFn1, runEffectFn1, runEffectFn3)
import Effect.Exception (Error, throw)


-- * Types

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
  sendMany :: forall loc
            . aux
           -> Socket from to loc
           -> NonEmpty Array Buffer
           -> Effect Unit

sendJson :: forall from to aux a loc
          . Sendable from to aux
         => EncodeJson a
         => aux
         -> Socket from to loc
         -> a
         -> Effect Unit
sendJson a s x = do
  case Argonaut.toString (encodeJson x) of
    Nothing -> throw "Can't show json"
    Just jsonString -> do
      buf <- Buffer.fromString jsonString Buffer.UTF8
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
    empty <- fromArray []
    sendMany' s $ NonEmpty addr $ [empty, x] <> xs

instance sendableRepDealer :: Sendable Rep Dealer Unit where
  sendMany _ s xs = sendMany' s xs

instance sendableDealerRep :: Sendable Dealer Rep Unit where
  sendMany _ s (NonEmpty x xs) = do
    empty <- fromArray []
    sendMany' s $ NonEmpty empty $ [x] <> xs

instance sendableDealerRouter :: Sendable Dealer Router Unit where
  sendMany _ s (NonEmpty x xs) = do
    empty <- fromArray []
    sendMany' s $ NonEmpty empty $ [x] <> xs

instance sendableRouterDealer :: Sendable Router Dealer ZMQIdent where
  sendMany (ZMQIdent addr) s (NonEmpty x xs) = do
    empty <- fromArray []
    sendMany' s $ NonEmpty addr $ [empty, x] <> xs


class Receivable from to aux | from to -> aux where
  readMany :: forall loc
            . Socket from to loc
           -> Aff (Maybe { aux :: aux, msg :: NonEmpty Array Buffer })

readJson :: forall from to aux loc a
          . Receivable from to aux
         => DecodeJson a
         => Socket from to loc
         -> Aff (Maybe { aux :: aux, msg :: a })
readJson s = do
  mX <- readJson' s
  case mX of
    Nothing -> pure Nothing
    Just eX -> case eX of
      Left _ -> pure Nothing
      Right x -> pure (Just x)

readJson' :: forall from to aux loc a
           . Receivable from to aux
          => DecodeJson a
          => Socket from to loc
          -> Aff (Maybe (Either String { aux :: aux, msg :: a }))
readJson' s = do
  mX <- readMany s
  case mX of
    Nothing -> pure Nothing
    Just {aux,msg: NonEmpty msg _} -> do
      str <- liftEffect (Buffer.toString Buffer.UTF8 msg)
      case decodeJson =<< jsonParser str of
        Left e -> pure $ Just $ Left e
        Right x -> pure $ Just $ Right {aux,msg:x}

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

derive instance genericFlag :: Generic Flag _
derive newtype instance eqFlag :: Eq Flag

foreign import sendMore :: Flag
foreign import dontWait :: Flag


newtype MonitorEvent = MonitorEvent String

derive instance genericMonitorEvent :: Generic MonitorEvent _
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
derive instance genericZMQOption :: Generic ZMQ_Option _
derive newtype instance eqZMQOption :: Eq ZMQ_Option


foreign import zmqIdentity :: ZMQ_Option



foreign import data Socket :: Type -> Type -> Location -> Type


newtype ZMQIdent = ZMQIdent Buffer


-- * Functions

foreign import socketImpl :: forall from to loc
                           . EffectFn1 from (Socket from to loc)

socket :: forall from to loc
        . IsLegal from to
       => from -> to -> Effect (Socket from to loc)
socket from _ = runEffectFn1 socketImpl from


foreign import setOptionImpl :: forall from to loc
                              . EffectFn3
                                (Socket from to loc)
                                ZMQ_Option
                                Buffer
                                Unit

setOption :: forall from to loc
           . Socket from to loc -> ZMQ_Option -> Buffer -> Effect Unit
setOption = runEffectFn3 setOptionImpl


foreign import getOptionImpl :: forall from to loc
                              . EffectFn2
                                (Socket from to loc)
                                ZMQ_Option
                                (Nullable Buffer)

getOption :: forall from to loc
           . Socket from to loc -> ZMQ_Option -> Effect (Maybe Buffer)
getOption s o = toMaybe <$> runEffectFn2 getOptionImpl s o


foreign import bindImpl :: forall from to
                         . EffectFn3
                           (Socket from to Bound)
                           String
                           (EffectFn1 (Nullable Error) Unit)
                           Unit

bind' :: forall from to
       . Bindable from
      => Socket from to Bound -> String -> Aff Unit
bind' s addr = makeAff \resolve -> do
  runEffectFn3 bindImpl s addr $ mkEffectFn1 \mE -> case toMaybe mE of
    Nothing -> resolve (Right unit)
    Just e -> resolve (Left e)
  pure nonCanceler


foreign import bindSyncImpl :: forall from to
                             . EffectFn2
                               (Socket from to Bound)
                               String
                               Unit

bindSync :: forall from to. Bindable from => Socket from to Bound -> String -> Effect Unit
bindSync = runEffectFn2 bindSyncImpl


foreign import unbindImpl :: forall from to
                           . EffectFn3
                             (Socket from to Bound)
                             String
                             (EffectFn1 (Nullable Error) Unit)
                             Unit

unbind :: forall from to. Bindable from => Socket from to Bound -> String -> Aff Unit
unbind s addr = makeAff \resolve -> do
  runEffectFn3 bindImpl s addr $ mkEffectFn1 \mE -> case toMaybe mE of
    Nothing -> resolve (Right unit)
    Just e -> resolve (Left e)
  pure nonCanceler


foreign import unbindSyncImpl :: forall from to
                               . EffectFn2
                                 (Socket from to Bound)
                                 String
                                 Unit

unbindSync :: forall from to. Bindable from => Socket from to Bound -> String -> Effect Unit
unbindSync = runEffectFn2 unbindSyncImpl


foreign import connectImpl :: forall from to
                            . EffectFn2
                              (Socket from to Connected)
                              String
                              Unit

connect :: forall from to. Connectable from => Socket from to Connected -> String -> Effect Unit
connect = runEffectFn2 connectImpl


foreign import disconnectImpl :: forall from to
                               . EffectFn2
                                 (Socket from to Connected)
                                 String
                                 Unit

disconnect :: forall from to. Connectable from => Socket from to Connected -> String -> Effect Unit
disconnect = runEffectFn2 disconnectImpl


foreign import subscribeImpl :: forall from to loc
                              . EffectFn2
                                (Socket from to loc)
                                String
                                Unit

subscribe :: forall from to loc. Subscriber from => Socket from to loc -> String -> Effect Unit
subscribe = runEffectFn2 subscribeImpl


foreign import unsubscribeImpl :: forall from to loc
                                . EffectFn2
                                  (Socket from to loc)
                                  String
                                  Unit

unsubscribe :: forall from to loc. Subscriber from => Socket from to loc -> String -> Effect Unit
unsubscribe = runEffectFn2 unsubscribeImpl


-- foreign import sendImpl :: forall eff from to loc
--                          . EffectFn3 (zeromq :: ZEROMQ | eff)
--                            (Socket from to loc)
--                            (Nullable Flag)
--                            Buffer
--                            Unit


-- send :: forall eff from to loc. Socket from to loc -> Maybe Flag -> Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit
-- send s f x = runEffectFn3 sendImpl s (toNullable f) x


foreign import sendManyImpl :: forall from to loc
                             . EffectFn2
                               (Socket from to loc)
                               (Array Buffer)
                               Unit


sendMany' :: forall from to loc
           . Socket from to loc
          -> NonEmpty Array Buffer
          -> Effect Unit
sendMany' s (NonEmpty x xs) = runEffectFn2 sendManyImpl s ([x] <> xs)


-- foreign import readImpl :: forall eff from to loc
--                          . EffectFn1 (zeromq :: ZEROMQ | eff)
--                            (Socket from to loc)
--                            (Nullable (Array Buffer))

-- readSync :: forall eff from to loc. Socket from to loc -> Eff (zeromq :: ZEROMQ | eff) (Maybe (NonEmpty Array Buffer))
-- readSync s = do
--   mxs <- runEffectFn1 readImpl s
--   case toMaybe mxs of
--     Nothing -> pure Nothing
--     Just xs -> case Array.uncons xs of
--       Nothing -> pure Nothing
--       Just {head,tail} -> pure $ Just $ NonEmpty head tail


foreign import monitorImpl :: forall from to loc
                            . EffectFn3
                              (Socket from to loc)
                              Milliseconds
                              Int
                              Unit


monitor :: forall from to loc. Socket from to loc -> Milliseconds -> Int -> Effect Unit
monitor = runEffectFn3 monitorImpl


foreign import unmonitorImpl :: forall from to loc
                              . EffectFn1
                                (Socket from to loc)
                                Unit

unmonitor :: forall from to loc. Socket from to loc -> Effect Unit
unmonitor = runEffectFn1 unmonitorImpl


foreign import addMonitorListenerImpl :: forall from to loc
                                       . EffectFn3
                                         (Socket from to loc)
                                         MonitorEvent
                                         (EffectFn1 Foreign Unit)
                                         Unit

addMonitorListener :: forall from to loc
                    . Socket from to loc
                   -> MonitorEvent
                   -> (Foreign -> Effect Unit)
                   -> Effect Unit
addMonitorListener s e f = runEffectFn3 addMonitorListenerImpl s e (mkEffectFn1 f)


foreign import removeAllMonitorListenersImpl :: forall from to loc
                                              . EffectFn2
                                                (Socket from to loc)
                                                MonitorEvent
                                                Unit

removeAllMonitorListeners :: forall from to loc. Socket from to loc -> MonitorEvent -> Effect Unit
removeAllMonitorListeners = runEffectFn2 removeAllMonitorListenersImpl


foreign import closeImpl :: forall from to loc
                          . EffectFn1
                            (Socket from to loc)
                            Unit

close :: forall from to loc. Socket from to loc -> Effect Unit
close = runEffectFn1 closeImpl


foreign import receiveImpl :: forall from to loc
                            . EffectFn2
                              (Socket from to loc)
                              (EffectFn1 (Array Buffer) Unit)
                              Unit

read' :: forall from to loc
       . Socket from to loc
      -> Aff (Array Buffer)
read' s = makeAff \resolve -> do
  runEffectFn2 receiveImpl s (mkEffectFn1 \xs -> resolve (Right xs))
  pure nonCanceler


foreign import addReceiveListenerImpl :: forall from to loc
                                       . EffectFn2
                                         (Socket from to loc)
                                         (EffectFn1 (Array Buffer) Unit)
                                         Unit

addReceiveListener :: forall from to loc
                    . Socket from to loc
                   -> (Array Buffer -> Effect Unit)
                   -> Effect Unit
addReceiveListener s f = runEffectFn2 addReceiveListenerImpl s (mkEffectFn1 f)


foreign import removeAllReceiveListenersImpl :: forall from to loc
                                              . EffectFn1
                                                (Socket from to loc)
                                                Unit

removeAllReceiveListeners :: forall from to loc. Socket from to loc -> Effect Unit
removeAllReceiveListeners = runEffectFn1 removeAllReceiveListenersImpl


foreign import proxyImpl :: forall from1 to1 from2 to2 from3 to3 loc3
                          . EffectFn3
                            (Socket from1 to1 Bound)
                            (Socket from2 to2 Bound)
                            (Nullable (Socket from3 to3 loc3))
                            Unit

proxy :: forall from1 to1 from2 to2 from3 to3 loc3
       . Socket from1 to1 Bound -- ^ Frontend
      -> Socket from2 to2 Bound -- ^ Backend
      -> Maybe (Socket from3 to3 loc3)
      -> Effect Unit
proxy f b c = runEffectFn3 proxyImpl f b (toNullable c)
