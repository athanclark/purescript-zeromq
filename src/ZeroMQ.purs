module ZeroMQ
  ( ZEROMQ, registerProducer, registerWorker, registerPublisher, registerSubscriber
  ) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI (Authority, URI (..), HierarchicalPart (..), Scheme (..))
import Data.URI.URI as URI
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, runEffFn2, mkEffFn1, runEffFn1, runEffFn3)
import Node.Buffer (Buffer)


foreign import data ZEROMQ :: Effect

foreign import registerProducerImpl :: forall eff. EffFn2 (zeromq :: ZEROMQ | eff)
                 String
                 ( EffFn1 (zeromq :: ZEROMQ | eff)
                     (EffFn1 (zeromq :: ZEROMQ | eff) Buffer Unit)
                     Unit
                 )
                 Unit

registerProducer :: forall eff
                  . Authority
                 -> ( (Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit)
                   -> Eff (zeromq :: ZEROMQ | eff) Unit
                    )
                 -> Eff (zeromq :: ZEROMQ | eff) Unit
registerProducer authority f =
  runEffFn2 registerProducerImpl (URI.print uri) (mkEffFn1 $ f <<< runEffFn1)
  where
    h = HierarchicalPart (Just authority) Nothing
    uri = URI (Just (Scheme "tcp")) h Nothing Nothing

foreign import registerWorkerImpl :: forall eff. EffFn2 (zeromq :: ZEROMQ | eff)
                 String
                 ( EffFn1 (zeromq :: ZEROMQ | eff) Buffer Unit
                 )
                 Unit

registerWorker :: forall eff
                . Authority
               -> (Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit)
               -> Eff (zeromq :: ZEROMQ | eff) Unit
registerWorker authority f =
  runEffFn2 registerWorkerImpl (URI.print uri) (mkEffFn1 f)
  where
    h = HierarchicalPart (Just authority) Nothing
    uri = URI (Just (Scheme "tcp")) h Nothing Nothing


foreign import registerPublisherImpl :: forall eff. EffFn2 (zeromq :: ZEROMQ | eff)
                 String
                 ( EffFn1 (zeromq :: ZEROMQ | eff)
                     (EffFn2 (zeromq :: ZEROMQ | eff) String String Unit)
                     Unit
                 )
                 Unit

registerPublisher :: forall eff
                   . Authority
                  -> ( (String -> String -> Eff (zeromq :: ZEROMQ | eff) Unit)
                    -> Eff (zeromq :: ZEROMQ | eff) Unit
                     )
                  -> Eff (zeromq :: ZEROMQ | eff) Unit
registerPublisher authority f =
  runEffFn2 registerPublisherImpl (URI.print uri) (mkEffFn1 $ f <<< runEffFn2)
  where
    h = HierarchicalPart (Just authority) Nothing
    uri = URI (Just (Scheme "tcp")) h Nothing Nothing

foreign import registerSubscriberImpl :: forall eff. EffFn3 (zeromq :: ZEROMQ | eff)
                 String
                 String
                 ( EffFn1 (zeromq :: ZEROMQ | eff) Buffer Unit
                 )
                 Unit

registerSubscriber :: forall eff
                    . Authority
                   -> String
                   -> (Buffer -> Eff (zeromq :: ZEROMQ | eff) Unit)
                   -> Eff (zeromq :: ZEROMQ | eff) Unit
registerSubscriber authority channel f =
  runEffFn3 registerSubscriberImpl (URI.print uri) channel (mkEffFn1 f)
  where
    h = HierarchicalPart (Just authority) Nothing
    uri = URI (Just (Scheme "tcp")) h Nothing Nothing
