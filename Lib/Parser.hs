module Lib.Parser (
  encodePacket
, encodePayload
, decodePacket
, decodePayload
) where

import Lib.Packet
import Lib.Utils
import List (elemIndex)
import Data.List (intercalate)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust, listToMaybe, fromJust)
import Text.JSON
import Data.Monoid
import Control.Applicative
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Text.Regex.Posix
import Control.Monad.State
import Debug.Trace

reasonsList = [ "transport not supported"
              , "client not handshaken"
              , "unauthorized"
              ]

adviceList = ["reconnect"]

findList :: [String] -> Maybe String -> Maybe String
findList _ Nothing = Nothing
findList xs (Just str) = show <$> str `elemIndex` xs

resolveList :: [String] -> Maybe String -> Maybe String
resolveList _ Nothing = Nothing
resolveList xs (Just str) = stringOrNothing xs $ read str

stringOrNothing:: [String] -> Int -> Maybe String
stringOrNothing xs i
  | i < 0 || i > len - 1 = Nothing
  | otherwise = let el = xs !! i in if el == "" then Nothing else Just el
  where len = length xs

getPieces :: String -> String -> [String]
getPieces str regex = (\(_, _, _, x) -> x) (str =~ regex :: (String, String, String, [String]))

-- Encodes a packet.
encodePacket :: Packet -> String
encodePacket packet
  | kind' == Message = encode $ dat packet
  | kind' == Json = encode $ Text.JSON.encode <$> (dat packet)
  | kind' == Connect = encode $ qs packet
  | kind' == Event =
    let dat = if packet' == [] then Nothing else Just . Text.JSON.encode . toJSObject $ packet'
        packet' = packetName ++ packetArgs
        packetName = if name packet == Nothing then [] else [("name", showJSON . fromJust $ name packet)]
        packetArgs = if args packet == [] then [] else [("args", showJSON $ args packet)]
    in encode $ dat
  | kind' == Ack =
    encode . Just $ (fromMaybe "" (ackId packet))
    ++ if (>0) . length $ args packet
       then "+" ++ (Text.JSON.encode . showJSON $ args packet)
       else ""
  | kind' == Lib.Packet.Error =
    let reason' = findList reasonsList $ reason packet
        advice' = (:) '+' <$> findList adviceList (advice packet)
    in encode $ reason' `mappend` advice'
  | otherwise = encode Nothing
  where kind' = kind packet
        id' = maybe "" show $ packetId packet
        ack' = if ack packet == "data" then "+" else ""
        endpoint' = endpoint packet
        baseString = intercalate ":" [show $ fromEnum kind', id' ++ ack', fromMaybe "" endpoint']
        encode Nothing = baseString
        encode (Just dat) = baseString ++ ":" ++ dat

-- decodes a packet
decodePacket :: String -> Packet
decodePacket str
  | kind' == Message = packet {dat= dat'}
  | kind' == Connect = packet {qs= dat'}
  | kind' == Json = packet {dat= dat'}
  | kind' == Ack
    = let pieces = getPieces (fromMaybe "" dat') "^([0-9]+)(\\+)?(.*)"
          ackId' = stringOrNothing pieces 0
          args' = let str_args = stringOrNothing pieces 2
                      decodeResult (Just a) = decode a :: Result [JSValue]
                      decodeResult _ = Ok []
                      getResult (Ok x) = x
                      getResult _ = []
                  in getResult $ decodeResult str_args
      in packet {ackId= ackId', args= args'}
  | kind' == Lib.Packet.Error
    = let pieces = split (== '+') <$> dat'
          getPiece int (Just a)= if int < length a then Just $ a !! int else Nothing
          getPiece _ _ = Nothing
      in packet { reason= resolveList reasonsList $ getPiece 0 pieces
                , advice= resolveList adviceList $ getPiece 1 pieces}
  | kind' == Event = let decodedEvent = if isJust dat'
                                        then decode $ fromJust dat'
                                        else Text.JSON.Error "No data given"
                         obj (Ok (JSObject a)) = fromJSObject a
                         obj (Text.JSON.Error _) = []
                         maybeResult (Just (Ok a)) = Just a
                         maybeResult _ = Nothing
                     in packet { name= maybeResult $ readJSON <$> lookup "name" (obj decodedEvent)
                               , args= fromMaybe [] . maybeResult $ readJSON <$> lookup "args" (obj decodedEvent)}
  | otherwise = packet
  where pieces = getPieces str regex
        regex = "([^:]+):([0-9]+)?(\\+)?:([^:]+)?:?(.*)?"
        kind' = (toEnum . read $ pieces !! 0) :: PacketKind
        packetId' = read <$> (stringOrNothing pieces 1)
        dat' = stringOrNothing pieces 4
        packet = defPacket { packetId= packetId'
                           , kind= kind'
                           , endpoint= stringOrNothing pieces 3
                           , ack= if (isJust packetId') && (isJust $ stringOrNothing pieces 2)
                                  then "data"
                                  else ""
                           }

-- encodes payload
encodePayload :: [String] -> String
encodePayload (packet:[]) = packet
encodePayload packets = concat $ map (\packet -> "\xfffd" ++ (show $ length packet) ++ "\xfffd" ++ packet) packets

parsePayload :: String -> State ([Packet], Int, String) [Packet]
parsePayload xs = do
  (packets, i, len) <- get
  let x = xs !! i
  if x == '\xfffd'
  then do
    let packet = decodePacket (take (read len) $ drop (i + 1) xs)
    put (packets ++ [packet], i + 2 + (read len), "")
  else do
    put (packets, i + 1, len ++ [x])

  (packets, i, len) <- get

  if i <= length xs
  then parsePayload xs
  else return packets

-- decodes payload
decodePayload :: String -> [Packet]
decodePayload "" = []
decodePayload ('\xfffd':xs) = evalState (parsePayload xs) ([], 0, "")
decodePayload xs = [decodePacket xs]
