module Procedure.Config exposing
  ( Msg
  , Model
  , init
  , update
  , subscriptions
  )

{-| Configure your application to use procedures.

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
      = ProcMsg (Procedure.Config.Msg AppMsg)

-}
type alias Msg msg
  = Procedure.Internal.Msg msg


{-| Represents the internal state used to track running procedures. 

You should store this in your application's model like so:

    type alias AppModel =
      { procModel : Procedure.Config.Model AppMsg
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
          Procedure.Config.update procMsg appModel.procModel
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
    Subscribe procedureId nextMessage subGenerator ->
      ( updateProcedureModel (addChannel subGenerator) procedureId registry
      , sendMessageAfter 0 nextMessage
      )
    Unsubscribe procedureId channelId nextMessage ->
      ( updateProcedureModel (deleteChannel channelId) procedureId registry
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


updateProcedureModel : (ProcedureModel msg -> ProcedureModel msg) -> ProcedureId -> Registry msg -> Registry msg
updateProcedureModel mapper procedureId registry =
  let
    procModel =
      registry.procedures
        |> Dict.get procedureId
        |> Maybe.withDefault defaultProcedureModel
        |> mapper
  in
    { registry | procedures = Dict.insert procedureId procModel registry.procedures }


sendMessageAfter : Float -> msg -> Cmd msg
sendMessageAfter timeout msg =
  Process.sleep timeout
    |> Task.perform (always msg)


{-| Get any subscriptions necessary for running procedures. 

Add this to your application's subscriptions function like so:

    appSubscriptions : AppModel -> Sub AppMsg
    appSubscriptions appModel =
      Procedure.Config.subscriptions appModel.procModel

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
