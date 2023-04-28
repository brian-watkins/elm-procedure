module Procedure.Channel exposing
  ( Channel
  , ChannelKey
  , ChannelRequest
  , open
  , join
  , connect
  , filter
  , accept
  , acceptOne
  , acceptUntil
  )

{-| A channel represents a method for receiving messages from the outside world.

@docs Channel

# Define a Channel

You can define a channel that simply listens for messages on a subscription.

@docs join

----

You can also define a channel by providing a command and a subscription to receive messages in response. 

@docs ChannelKey, ChannelRequest, open, connect

# Work with a Channel
@docs filter

# Use a Channel in a Procedure
@docs accept, acceptOne, acceptUntil

-}

import Task
import Procedure.Internal exposing (ChannelId, Procedure(..), Msg(..))


{-| Represents the unique key assigned to each procedure.

This is most useful when opening a channel by sending a command, where you might pass the key
through a port and then use it to filter incoming messages to a subscription.
-}
type alias ChannelKey =
  String


{-| Represents a method for receiving messages from the outside world. 
-}
type Channel a msg
  = Channel
    { request : ChannelKey -> Cmd msg
    , subscription : (a -> msg) -> Sub msg
    , shouldAccept : ChannelKey -> a -> Bool
    }


{-| Represents a request to open a channel.
-}
type ChannelRequest msg
  = ChannelRequest (ChannelKey -> Cmd msg)


{-| Open a channel by sending a command. Use this in conjunction with `connect` to define a channel.

For example, you might need to send a command over a port and then wait for a response via a port subscription. 
You could accomplish that like so:

    Channel.open (\_ -> myPortCommand)
      |> Channel.connect myPortSubscription
      |> Channel.acceptOne
      |> Procedure.run ProcedureTagger DataTagger

Use the provided `ChannelKey` if you need something to connect subscription messages with the command that opens the channel.
On the JS side, your port command could take the channel key and pass it back when supplying a subscription message.
In your channel setup, you would filter messages by this key. See `filter` for an example.

-}
open : (ChannelKey -> Cmd msg) -> ChannelRequest msg
open =
  ChannelRequest


{-| Define a channel by providing a subscription to receive messages after
you use `open` to send a command that opens a channel.

-}
connect : ((a -> msg) -> Sub msg) -> ChannelRequest msg -> Channel a msg
connect generator (ChannelRequest requestGenerator) =
  Channel
    { request = requestGenerator
    , subscription = generator
    , shouldAccept = defaultPredicate
    }


{-| Define a channel by providing a subscription to receive messages.

For example, you might want to listen for key presses but only send certain
ones to your update function. You could accomplish that like so:

    Channel.join (\tagger ->
      Decode.map tagger keyDecoder
        |> Browser.Events.onKeyPress 
    )
    |> Channel.filter (\_ keyPress -> 
      keyPress == "Z"
        || keyPress == "X"
        || keyPress == "Y"
    )
    |> Channel.acceptUntil (\_ -> False)
    |> Procedure.run ProcMsg PressedKey

-}
join : ((a -> msg) -> Sub msg) -> Channel a msg
join generator =
  Channel
    { request = defaultRequest
    , subscription = generator
    , shouldAccept = defaultPredicate
    }


{-| Filter messages received by whatever subscription is listening on this channel.

For example, you might need to send a command over a port and then wait for a response via a port subscription. 
You could accomplish that like so:

    Channel.open (\key -> myPortCommand key)
      |> Channel.connect myPortSubscription
      |> Channel.filter (\key data -> data.key == key)
      |> Channel.acceptOne
      |> Procedure.run ProcedureTagger DataTagger

In this example, we pass the channel key through the port and use it to filter incoming subscription messages. This
allows us to associate the messages we receive with a particular channel (and procedure), in case multiple procedures
with channels that utilize this subscription are running simultaneously. 

Note: Calling filter multiple times on a channel simply replaces any existing filter on that channel.

-}
filter : (ChannelKey -> a -> Bool) -> Channel a msg -> Channel a msg
filter predicate (Channel channel) =
  Channel 
    { channel | shouldAccept = predicate }


{-| Generate a procedure that accepts the first message to be processed from a channel. After
that message is processed, the channel is closed. 

For example, if you wanted to send a request via a port command and wait for a response on some port subscription,
you could do the following:

    Channel.open (\_ -> myPortCommand)
      |> Channel.connect myPortSubscription
      |> Channel.acceptOne
      |> Procedure.run ProcedureTagger DataTagger

-}
acceptOne : Channel a msg -> Procedure e a msg
acceptOne =
  acceptUntil <| always True


{-| Generate a procedure that processes messages on a channel indefinitely.

For example, suppose `mySubscription` provides a stream of numbers. If you wanted to filter and map
these messages before passing these to your update function, you could do the following:

    Channel.join mySubscription
      |> Channel.filter (\_ data -> modBy 2 data == 0)
      |> Channel.accept
      |> Procedure.map String.fromInt
      |> Procedure.run ProcedureTagger StringTagger

Then, as numbers come in through `mySubscription`, a `StringTagger` message will be sent
that tags the even numbers as a string.

-}
accept : Channel a msg -> Procedure e a msg
accept =
  acceptUntil <| always False


{-| Generate a procedure that processes messages on a channel as they are received until the 
predicate is satisfied. When the predicate is satisfied, the last message received on the channel
will be processed and the channel will be closed. 

For example, suppose `mySubscription` provides a stream of numbers. If you wanted to filter and map 
these messages before passing these to your update function, you could do the following:

    Channel.join mySubscription
      |> Channel.filter (\_ data -> modBy 2 data == 0)
      |> Channel.acceptUntil (\data -> data == 20)
      |> Procedure.map String.fromInt
      |> Procedure.run ProcedureTagger StringTagger

Then, as numbers come in through `mySubscription`, a `StringTagger` message will be sent
that tags the number as a string. Once the number `20` is received from the subscription, the
channel will close and process no more messages. The last message sent by this procedure will
be `StringTagger "20"`.

-}
acceptUntil : (a -> Bool) -> Channel a msg -> Procedure e a msg
acceptUntil shouldUnsubscribe (Channel channel) =
  Procedure <|
    \procId msgTagger resultTagger ->
      let
        requestCommandMsg channelId =
          channel.request (channelKey channelId)
            |> msgTagger << Execute procId

        subGenerator channelId =
          channel.subscription <|
            \aData ->
              if channel.shouldAccept (channelKey channelId) aData then
                generateMsg channelId aData
              else
                msgTagger Continue

        generateMsg channelId aData =
          if shouldUnsubscribe aData then
            Ok aData
              |> resultTagger
              |> msgTagger << Unsubscribe procId channelId
          else
            Ok aData
              |> resultTagger
      in
        Task.succeed subGenerator
          |> Task.perform (msgTagger << Subscribe procId requestCommandMsg)


channelKey : ChannelId -> ChannelKey
channelKey channelId =
  "Channel-" ++ String.fromInt channelId


defaultRequest : ChannelKey -> Cmd msg
defaultRequest _ =
  Cmd.none


defaultPredicate : ChannelKey -> a -> Bool
defaultPredicate _ _ =
  True