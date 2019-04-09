module Procedure.Channel exposing
  ( ProcedureId
  , Channel
  , ChannelRequest
  , open
  , join
  , connect
  , filter
  , acceptOne
  , acceptUntil
  )

{-| A channel represents a method for receiving messages from the outside world.

You can open a channel by sending a command (usually via a port) or simply by providing
a subscription. Once open, you can filter messages received. See `acceptOne` and `acceptUntil` for
functions that allow you to incorporate a channel into a procedure. 

@docs Channel

# Define a Channel
@docs join

# Open a Channel and Connect
@docs ProcedureId, ChannelRequest, open, connect

# Work with a Channel
@docs filter

# Use a Channel in a Procedure
@docs acceptOne, acceptUntil

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


{-| Represents a request to open a channel.
-}
type ChannelRequest msg
  = ChannelRequest (ProcedureId -> Cmd msg)


{-| Open a channel by sending a command.
-}
open : (ProcedureId -> Cmd msg) -> ChannelRequest msg
open =
  ChannelRequest


{-| Provide a subscription to receive messages after a command has been sent to open the channel.
-}
connect : ((a -> msg) -> Sub msg) -> ChannelRequest msg -> Channel a msg
connect generator (ChannelRequest requestGenerator) =
  Channel
    { initialRequest = requestGenerator
    , messageHandler = generator
    , shouldProcessMessage = defaultPredicate
    }


{-| Define a channel by providing a subscription to receive messages.
-}
join : ((a -> msg) -> Sub msg) -> Channel a msg
join generator =
  Channel
    { initialRequest = emptyRequest
    , messageHandler = generator
    , shouldProcessMessage = defaultPredicate
    }


{-| Filter messages received by whatever subscription is listening on this channel.

Note: Calling filter multiple times on a channel simply replaces any existing filter on that channel.
-}
filter : (ProcedureId -> a -> Bool) -> Channel a msg -> Channel a msg
filter predicate (Channel channel) =
  Channel 
    { channel | shouldProcessMessage = predicate }


{-| Generate a step that accepts the first message to be processed from a channel. After
that message is processed, the channel is closed. 

For example, if you wanted to send a request via a port command and wait for a response on some port subscription,
you could do the following:

    Channel.open myPortCommand
      |> Channel.connect myPortSubscription
      |> Channel.acceptOne
      |> Procedure.run ProcedureTagger DataTagger

-}
acceptOne : Channel a msg -> Step e a msg
acceptOne =
  acceptUntil <|
    \_ -> True


{-| Generate a step that processes messages on a channel as they are received until the 
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
that tags the number as a string. When this procedure completes, the last message sent will
be `StringTagger "20"`. 

-}
acceptUntil : (a -> Bool) -> Channel a msg -> Step e a msg
acceptUntil isLastMessage (Channel channel) =
  Step <|
    \procId msgTagger resultTagger ->
      let
        requestCommandMsg =
          channel.initialRequest procId
            |> msgTagger << Execute procId

        subGenerator channelId =
          channel.messageHandler <|
            \aData ->
              if channel.shouldProcessMessage procId aData then
                if isLastMessage aData then
                  Ok aData
                    |> resultTagger
                    |> msgTagger << Unsubscribe procId channelId
                else
                  Ok aData
                    |> resultTagger
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