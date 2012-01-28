module Tests.Parser (tests) where

import Test.Framework
import Test.HUnit hiding (Test)
import Lib.Parser
import Lib.Packet
import Text.JSON hiding (Error)
import Utils

tests :: [Test]
tests = concat $

  [ fromAssertions "encoding error packet"
      ["7::" @=? encodePacket defPacket {kind= Error}]

  , fromAssertions "encoding error packet with reason"
     ["7:::0" @=? encodePacket defPacket {kind= Error, reason= Just "transport not supported"}]

  , fromAssertions "encoding error packet with reason and advice"
     ["7:::2+0" @=? encodePacket defPacket {kind= Error, reason= Just "unauthorized", advice= Just "reconnect"}]

  , fromAssertions "encoding error packet with endpoint"
     ["7::/woot" @=? encodePacket defPacket {kind= Error, endpoint= Just "/woot"}]

  , fromAssertions "encoding ack packet"
     ["6:::140" @=? encodePacket defPacket {kind= Ack, ackId= Just "140"}]

  , fromAssertions "encoding ack packet with args"
     ["6:::12+[\"woot\",\"wa\"]" @=? encodePacket defPacket {kind= Ack, ackId= Just "12", args= [showJSON "woot", showJSON "wa"]}]

  , fromAssertions "encoding json packet"
     ["4:::\"2\"" @=? encodePacket defPacket {kind= Json, dat= Just "2"}]

  , fromAssertions "encoding json packet with message id and ack data"
     ["4:1+::\"{\\\"a\\\": \\\"b\\\"}\"" @=? encodePacket defPacket {kind= Json, packetId= Just 1, ack= "data", dat= Just "{\"a\": \"b\"}"}]

  , fromAssertions "encoding an event packet"
     ["5:::{\"name\":\"woot\"}" @=? encodePacket defPacket {kind= Event, name= Just "woot"}]

  , fromAssertions "encoding an event packet with message id and ack"
     ["5:1+::{\"name\":\"tobi\"}" @=? encodePacket defPacket {kind= Event, packetId= Just 1, ack= "data", name= Just "tobi"}]

  , fromAssertions "encoding an event packet with data"
     ["5:::{\"name\":\"edwald\",\"args\":[{\"a\":\"b\"},2,\"3\"]}"
     @=? encodePacket defPacket {kind= Event, name= Just "edwald", args= [JSObject $ toJSObject [("a", showJSON "b")], showJSON (2 :: Int), showJSON "3"]}]

  , fromAssertions "encoding a message packet"
     ["3:::woot" @=? encodePacket defPacket {kind= Message, dat= Just "woot"}]

  , fromAssertions "encoding a message packet with id and endpoint"
     ["3:5:/tobi" @=? encodePacket defPacket {kind= Message, packetId= Just 5, ack= "true", endpoint= Just "/tobi"}]

  , fromAssertions "encoding a heartbeat packet"
     ["2::" @=? encodePacket defPacket {kind= Heartbeat}]

  , fromAssertions "encoding a connection packet"
     ["1::/tobi" @=? encodePacket defPacket {kind= Connect, endpoint= Just "/tobi"}]

  , fromAssertions "encoding a connection packet with query string"
     ["1::/test:?test=1" @=? encodePacket defPacket {kind= Connect, endpoint= Just "/test", qs= Just "?test=1"}]

  , fromAssertions "encoding a disconnection packet"
     ["0::/woot" @=? encodePacket defPacket {kind= Disconnect, endpoint= Just "/woot"}]

  , fromAssertions "test encoding a payload"
     ["\xfffd\&5\xfffd\&3:::5\xfffd\&7\xfffd\&3:::53d" @=? encodePayload [ encodePacket $ defPacket {kind= Message, dat= Just "5"}
                                                                         , encodePacket $ defPacket {kind= Message, dat= Just "53d"}]]

  , fromAssertions "decoding a message packet"
     [defPacket {kind= Message, dat= Just "woot"} @=? decodePacket "3:::woot"]

  , fromAssertions "decoding a message packet with id and endpoint"
     [defPacket {kind= Message, endpoint= Just "/tobi", packetId= Just 5} @=? decodePacket "3:5:/tobi:"]

  , fromAssertions "decoding a heartbeat packet"
     [defPacket {kind= Heartbeat} @=? decodePacket "2:::"]

  , fromAssertions "decoding a connection packet"
     [defPacket {kind= Connect, endpoint= Just "/tobi"} @=? decodePacket "1::/tobi"]

  , fromAssertions "decoding a connection packet with query string"
     [defPacket {kind= Connect, endpoint= Just "/test", qs= Just "?test=1"} @=? decodePacket "1::/test:?test=1"]

  , fromAssertions "decoding json packet"
     [defPacket {kind= Json, dat= Just "2"} @=? decodePacket "4:::2"]

  , fromAssertions "decoding json packet with message id and ack data"
     [defPacket {kind= Json, packetId= Just 1, ack= "data", dat= Just "{\"a\": \"b\"}"} @=? decodePacket "4:1+::{\"a\": \"b\"}"]

  , fromAssertions "decoding an event packet"
     [defPacket {kind= Event, name= Just "woot"} @=? decodePacket "5:::{\"name\": \"woot\"}"]

  , fromAssertions "decoding an event packet with message id and ack"
     [defPacket {kind= Event, ack= "data", packetId= Just 1, name= Just "tobi"} @=? decodePacket "5:1+::{\"name\": \"tobi\"}"]

  , fromAssertions "decoding an event packet with data"
     [defPacket {kind= Event, name= Just "edwald", args= [makeObj [("a", showJSON "b")], showJSON (2 :: Int), showJSON "3"]}
       @=? decodePacket "5:::{\"name\":\"edwald\",\"args\":[{\"a\":\"b\"},2,\"3\"]}"]

  , fromAssertions "decoding error packet"
     [defPacket {kind= Error} @=? decodePacket "7:::"]

  , fromAssertions "decoding error packet with reason"
     [defPacket {kind= Error, reason= Just "transport not supported"} @=? decodePacket "7:::0"]

  , fromAssertions "decoding error packet with reason and advice"
     [defPacket {kind= Error, reason= Just "unauthorized", advice= Just "reconnect"} @=? decodePacket "7:::2+0"]

  , fromAssertions "decoding error packet with endpoint"
     [defPacket {kind= Error, endpoint= Just "/woot"} @=? decodePacket "7::/woot"]

  , fromAssertions "decoding ack packet"
     [defPacket {kind= Ack, ackId= Just "140"} @=? decodePacket "6:::140"]

  , fromAssertions "decoding ack packet with args"
     [defPacket {kind= Ack, ackId= Just "12", args= [showJSON "woot", showJSON "wa"]} @=? decodePacket "6:::12+[\"woot\", \"wa\"]"]

  , fromAssertions "decoding ack packet with bad json"
     [defPacket {kind= Ack, ackId= Just "1"} @=? decodePacket "6:::1+{\"++]"]

  , fromAssertions "test decoding a payload"
     [[defPacket {kind= Message, dat= Just "5"}, defPacket {kind= Message, dat= Just "53d"}, defPacket {kind= Disconnect}]
       @=? decodePayload("\xfffd\&5\xfffd\&3:::5\xfffd\&7\xfffd\&3:::53d\xfffd\&3\xfffd\&0::")]
  ]
