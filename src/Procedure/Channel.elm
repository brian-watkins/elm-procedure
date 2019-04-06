module Procedure.Channel exposing
  ( ProcedureId
  , ChannelRequest
  , send
  , subscribe
  , receive
  , filter
  )

{-| A channel represents a method for receiving messages from the outside world.

You can open a channel by sending a command (usually via a port) or simply by providing
a subscription. Once open, you can filter messages received. See `Procedure.await` for
how to incorporate a channel into a procedure. 

# Initialize a Channel with a subscription
@docs subscribe

# Initialize a Channel with a command
@docs ProcedureId, ChannelRequest, send, receive

# Work with a Channel
@docs filter

-}

import Task
import Procedure.Internal exposing (ProcedureId, Channel(..))


{-| Represents the unique identifier assigned to each procedure.

This is most useful when opening a channel by sending a command, where you might pass the identifier
through a port and filter incoming messages to a subscription.
-}
type alias ProcedureId =
  Procedure.Internal.ProcedureId


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
-}
filter : (ProcedureId -> a -> Bool) -> Channel a msg -> Channel a msg
filter predicate (Channel channel) =
  Channel 
    { channel | predicate = predicate }


emptyRequest : ProcedureId -> Cmd msg
emptyRequest _ =
  Cmd.none


defaultPredicate : ProcedureId -> a -> Bool
defaultPredicate _ _ =
  True