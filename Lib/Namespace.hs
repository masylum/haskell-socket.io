module Lib.Namespace ( Packet (..)
                     , PacketKind (..)
                     , defPacket
                     ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Lens.Lazy ((~=), access, (%=), mapLens, (^=), (^.), (^%=), (^%%=), (^$))
import Data.Lens.Template (makeLenses)


data Flags = Flags { volatile :: Bool
                   , json :: Bool
                   , exceptions :: [Int]
                   , endpoint :: String
                   }
data Namespace = Namespace { name :: Maybe String
                           , auth :: Bool
                           , flags :: Flags
                           } deriving (Show, Eq)

$(makeLenses [''Flags, ''Namespace])

-- TODO: type?
flagsLense namespace fn = (namespace ^. flags) ^. fn

-- Gets a Socket for a given sid
getSocket :: Namespace -> Int -> Socket
getSocket = sockets namespace !! sid

-- Sets the default flags
setDefaultFlags :: Namespace -> Namespace
setDefaultFlags namespace = (flagsLense namespace endpoint) ^= (name namespace) .
                            (flagsLense namespace exceptions) ^= [] $ namespace

-- Gets a default Namespace
defNamespace :: Maybe String -> Namespace
defNamespace name = setDefaultFlags $ Namespace { name = fromMaybe "" name
                                                , auth = False
                                                }

-- Get a room
getRoom :: Maybe String -> String
getRoom room = fromMaybe "" $ ((:) $ '/') <$> room

-- Retrieves all clients as Socket instances as an array.
clients :: Namespace -> Maybe String ->[Socket]
clients namespace room =
  let room' = name namespace ++ (getRoom room)
      getRoom [] = []
      getRoom a = map (\x -> socket x namespace) a
  in getRoom . (lookup room') . rooms $ manager namespace

-- Access logger interface.
log :: Namespace -> Log
log = log . manager

-- Access store.
store :: Namespace -> Store
store = store . manager

-- JSON message flag.
json :: Namespace -> Namespace
json namespace = flagsLense namespace json ^= True

-- Volatile message flag.
volatile :: Namespace -> Namespace
volatile namespace = flagsLense namespace volatile ^= True

-- Overrides the room to relay messages to (flag).
setEndpoint :: Maybe String -> Namespace -> Namespace
setEndpoint room namespace = flagsLense namespace endpoint ^= (getRoom room)

-- Adds a session id we should prevent relaying messages to (flag).
except :: Int -> Namespace -> Namespace
except id namespace = flagsLense namespace exceptions ^%= (++ id)

-- Sends out a packet.
-- TODO: Implement
packet :: packet -> Namespace -> Namespace

-- Sends to everyone.
send :: Namespace -> Maybe String -> Packet
send namespace dat' = defPacket { kind = if flagsLense namespace json then "json" else "message"
                                , dat = dat'
                                }

-- Emits to everyone (override).
-- TODO: Implement

-- Retrieves or creates a write-only socket for a client, unless specified.
socket :: Namespace -> Int -> Bool -> Socket
socket namespace sid readable = fromMaybe
                                  (defSocket (manager namespace) sid namespace readable)
                                  getSocket namespace sid

-- Sets authorization for this namespace.
-- TODO: Implement
-- authorization :: Namespace -> ( String? -> Bool?) -> Namespace

-- Called when a socket disconnects entirely.
handleDisconnect :: Namespace -> Int -> String -> Bool -> Namespace
handleDisconnect namespace sid reason raiseOnDisconnect =
  let socket = getSocket namespace sid
      readable = fromMaybe False $ readable <?> socket
  in if readable
     then
       let namespace' = if raiseOnDisconnect
                        then onDisconect socket reason
                        else namespace
       in deleteSocket namespace' sid
     else namespace

-- Performs authentication.
-- TODO: Implement
authorize :: Namespace -> Maybe String -> ([Error, Boolean] -> ()) -> Namespace
authorize namespace dat fn =
  let auth' True = auth namespace
      auth' False = do
        fn () True
  in auth' auth namespace
