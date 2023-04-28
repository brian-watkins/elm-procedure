module Procedure.Program exposing
  ( Msg
  , Model
  , init
  , update
  , subscriptions
  )

{-| Use these functions to configure your program to run procedures.

@docs Msg, Model, init, update, subscriptions

-}

import Dict exposing (Dict)
import Task
import Procedure.Internal exposing (ChannelId, Msg(..))


{-| Represents the internal `Msg` values used to track the state of a procedure.

The type variable refers to the `Msg` type used by your application. 
You should provide a message type that wraps these values like so:

    type AppMsg
      = ProcMsg (Procedure.Program.Msg AppMsg)

-}
type alias Msg msg
  = Procedure.Internal.Msg msg


{-| Represents the internal state used to track running procedures. 

You should store this in your application's model like so:

    type alias AppModel =
      { procModel : Procedure.Program.Model AppMsg
      }

-}
type Model msg =
  Model (Registry msg)


type alias Registry msg =
  { nextId: ChannelId
  , channels: Dict ChannelId (Sub msg)
  }


{-| Generate the model used to track procedures.
-}
init : Model msg
init =
  Model <| 
    { nextId = 0
    , channels = Dict.empty
    }


{-| Update the state of running procedures.

You should add this to your application's update function like so:

    update : AppMsg -> AppModel -> (AppModel, Cmd AppMsg)
    update appMsg appModel =
      case appMsg of
        ProcedureTagger procMsg ->
          Procedure.Program.update procMsg appModel.procModel
            |> Tuple.mapFirst (\updated -> { appModel | procModel = updated })

-}
update : Msg msg -> Model msg -> (Model msg, Cmd msg)
update msg (Model registry) =
  updateProcedures msg registry
    |> Tuple.mapFirst Model


updateProcedures : Msg msg -> Registry msg -> (Registry msg, Cmd msg)
updateProcedures msg registry =
  case msg of
    Initiate generator ->
      ( { registry | nextId = registry.nextId + 1 }
      , generator registry.nextId
      )
    Execute _ cmd ->
      ( registry
      , cmd 
      )
    Subscribe _ messageGenerator subGenerator ->
      ( addChannel subGenerator registry
      , messageGenerator registry.nextId
          |> sendMessage
      )
    Unsubscribe _ channelId nextMessage ->
      ( deleteChannel channelId registry
      , sendMessage nextMessage
      )
    Continue ->
      ( registry, Cmd.none )


addChannel : (ChannelId -> Sub msg) -> Registry msg -> Registry msg
addChannel subGenerator registry =
  { registry
  | nextId = registry.nextId + 1
  , channels = Dict.insert registry.nextId (subGenerator registry.nextId) registry.channels
  }


deleteChannel : ChannelId -> Registry msg -> Registry msg
deleteChannel channelId procModel =
  { procModel | channels = Dict.remove channelId procModel.channels }


sendMessage : msg -> Cmd msg
sendMessage msg =
  Task.succeed ()
    |> Task.perform (always msg)


{-| Get any subscriptions necessary for running procedures. 

Add this to your application's subscriptions function like so:

    appSubscriptions : AppModel -> Sub AppMsg
    appSubscriptions appModel =
      Procedure.Program.subscriptions appModel.procModel

Note: You only need to use this function if you are using procedures 
with channels, i.e. if you have subscriptions in your procedures.

-}
subscriptions : Model msg -> Sub msg
subscriptions (Model registry) =
  Dict.values registry.channels
    |> Sub.batch
