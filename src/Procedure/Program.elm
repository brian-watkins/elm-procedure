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
import Process
import Task
import Procedure.Internal exposing (ProcedureId, ChannelId, Msg(..))


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
  { nextId: ProcedureId
  , procedures: Dict ProcedureId (ProcedureModel msg)
  }


{-| Generate the model used to track procedures.
-}
init : Model msg
init =
  Model <| 
    { nextId = 0
    , procedures = Dict.empty
    }


type alias ProcedureModel msg =
  { nextId: ChannelId
  , channels: Dict ChannelId (Sub msg)
  }

defaultProcedureModel : ProcedureModel msg
defaultProcedureModel =
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
    Execute procedureId cmd ->
      ( registry, cmd )
    Subscribe procedureId messageGenerator subGenerator ->
      ( addChannel subGenerator
          |> updateProcedureModel procedureId registry
      , nextChannelIdForProcedure procedureId registry
          |> messageGenerator
          |> sendMessageAfter 0
      )
    Unsubscribe procedureId channelId nextMessage ->
      ( deleteChannel channelId
          |> updateProcedureModel procedureId registry
      , sendMessageAfter 0 nextMessage
      )
    Continue ->
      ( registry, Cmd.none )


addChannel : (ChannelId -> Sub msg) -> ProcedureModel msg -> ProcedureModel msg
addChannel subGenerator procModel =
  { procModel
  | nextId = procModel.nextId + 1
  , channels = Dict.insert procModel.nextId (subGenerator procModel.nextId) procModel.channels
  }


deleteChannel : ChannelId -> ProcedureModel msg -> ProcedureModel msg
deleteChannel channelId procModel =
  { procModel | channels = Dict.remove channelId procModel.channels }


updateProcedureModel : ProcedureId -> Registry msg -> (ProcedureModel msg -> ProcedureModel msg) -> Registry msg
updateProcedureModel procedureId registry mapper =
  let
    procModel =
      registry.procedures
        |> Dict.get procedureId
        |> Maybe.withDefault defaultProcedureModel
        |> mapper
  in
    { registry | procedures = Dict.insert procedureId procModel registry.procedures }


nextChannelIdForProcedure : ProcedureId -> Registry msg -> ChannelId
nextChannelIdForProcedure procedureId registry =
  registry.procedures
    |> Dict.get procedureId
    |> Maybe.withDefault defaultProcedureModel
    |> .nextId


sendMessageAfter : Float -> msg -> Cmd msg
sendMessageAfter timeout msg =
  Process.sleep timeout
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
  Dict.values registry.procedures
    |> List.map .channels
    |> List.map Dict.values
    |> List.concat
    |> Sub.batch
