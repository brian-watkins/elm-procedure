module Procedure.Channel exposing
  ( ProcedureId
  , Channel
  , ChannelRequest
  , send
  , subscribe
  , receive
  , filter
  , open
  , await
  )

{-| A channel represents a method for receiving messages from the outside world.

You can open a channel by sending a command (usually via a port) or simply by providing
a subscription. Once open, you can filter messages received. See `await` and `open` for
functions that allow you to incorporate a channel into a procedure. 

@docs Channel

# Initialize a Channel with a subscription
@docs subscribe

# Initialize a Channel with a command
@docs ProcedureId, ChannelRequest, send, receive

# Work with a Channel
@docs filter

# Use a Channel in a Procedure
@docs await, open

-}

import Task
import Procedure.Internal exposing (ProcedureId, ChannelId, Channel, Step(..), Msg(..))


{-| Represents the unique identifier assigned to each procedure.

This is most useful when opening a channel by sending a command, where you might pass the identifier
through a port and then use it to filter incoming messages to a subscription.
-}
type alias ProcedureId =
  Procedure.Internal.ProcedureId


{-| Represents a method for receiving messages from the outside world. 
-}
type Channel a msg
  = Channel (Procedure.Internal.Channel a msg)


{-| Represents a request to receive messages over a channel.
-}
type ChannelRequest msg
  = ChannelRequest (ProcedureId -> Cmd msg)


{-| Open a channel by sending a command message.
-}
send : (ProcedureId -> Cmd msg) -> ChannelRequest msg
send =
  ChannelRequest


{-| Register a subscription to receive messages after a command has been sent across the channel.
-}
receive : ((a -> msg) -> Sub msg) -> ChannelRequest msg -> Channel a msg
receive generator (ChannelRequest requestGenerator) =
  Channel
    { requestGenerator = requestGenerator
    , receiver = generator
    , predicate = defaultPredicate
    }


{-| Initiate an exchange across a channel by registering a subscription to receive messages.
-}
subscribe : ((a -> msg) -> Sub msg) -> Channel a msg
subscribe generator =
  Channel
    { requestGenerator = emptyRequest
    , receiver = generator
    , predicate = defaultPredicate
    }


{-| Filter messages received by whatever subscription is listening on this channel.

Note: Calling filter multiple times on a channel simply replaces any existing filter on that channel.
-}
filter : (ProcedureId -> a -> Bool) -> Channel a msg -> Channel a msg
filter predicate (Channel channel) =
  Channel 
    { channel | predicate = predicate }


{-| Generate a step that opens a `Channel` and waits for the first message to be processed.

For example, if you wanted to send a request via a port command and wait for a response on some port subscription,
you could do the following:

    Channel.send myPortCommand
      |> Channel.receive myPortSubscription
      |> Channel.await
      |> Procedure.run ProcedureTagger DataTagger

-}
await : Channel a msg -> Step e a msg
await =
  consumeChannel <| 
    \procId channelId msgTagger resultTagger data ->
      Ok data
        |> resultTagger
        |> msgTagger << Unsubscribe procId channelId


{-| Generate a step that opens a `Channel` and processes messages as they are received.

For example, if you wanted to filter and map messages received over a subscription before passing these
to your update function, you could do the following:

    Channel.subscribe mySubscription
      |> Channel.filter (\_ data -> modBy 2 data == 0)
      |> Channel.open
      |> Procedure.map String.fromInt
      |> Procedure.run ProcedureTagger StringTagger

Then, as numbers come in through `mySubscription`, a `StringTagger` message will be sent
that tags the number as a string.

-}
open : Channel a msg -> Step e a msg
open =
  consumeChannel <|
    \_ _ _ resultTagger data ->
      resultTagger <| Ok data


consumeChannel : (ProcedureId -> ChannelId -> (Msg msg -> msg) -> (Result e a -> msg) -> a -> msg) 
  -> Channel a msg 
  -> Step e a msg
consumeChannel dataTagger (Channel channel) =
  Step <|
    \procId msgTagger resultTagger ->
      let
        requestCommandMsg =
          channel.requestGenerator procId
            |> msgTagger << Execute procId

        subGenerator channelId =
          channel.receiver <|
            \aData ->
              if channel.predicate procId aData then
                dataTagger procId channelId msgTagger resultTagger aData
              else
                msgTagger Continue
      in
        Task.succeed subGenerator
          |> Task.perform (msgTagger << Subscribe procId requestCommandMsg)


emptyRequest : ProcedureId -> Cmd msg
emptyRequest _ =
  Cmd.none


defaultPredicate : ProcedureId -> a -> Bool
defaultPredicate _ _ =
  True