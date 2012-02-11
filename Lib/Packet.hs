module Lib.Packet ( Packet (..)
                  , PacketKind (..)
                  , defPacket
                  ) where

import Text.JSON

data PacketKind = Disconnect | Connect | Heartbeat | Message | Json | Event | Ack | Error | Noop
  deriving (Show, Enum, Eq)

data Packet = Packet { kind :: PacketKind
                     , packetId :: Maybe Int
                     , name :: Maybe String
                     , endpoint :: Maybe String
                     , ack :: String
                     , ackId :: Maybe String
                     , qs :: Maybe String
                     , args :: [JSValue]
                     , dat :: Maybe String
                     , reason :: Maybe String
                     , advice :: Maybe String
                     } deriving (Show, Eq)

defPacket :: Packet
defPacket = Packet { kind = undefined
                   , packetId = Nothing
                   , name = Nothing
                   , endpoint = Nothing
                   , ack = ""
                   , ackId = Nothing
                   , qs = Nothing
                   , args = []
                   , dat = Nothing
                   , reason = Nothing
                   , advice = Nothing
                   }
