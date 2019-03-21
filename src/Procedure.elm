module Procedure exposing
  ( Msg
  , Model
  , Step
  , defaultModel
  , do
  , get
  , send
  , break
  , catch
  , andThen
  , map
  , mapError
  , sequence
  , waitFor
  , waitForValue
  , subscriptions
  , update
  , try
  , run
  )

import Task
import Dict exposing (Dict)


type alias Step e a msg =
  ProcedureId -> (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg


get : ((a -> msg) -> Cmd msg) -> Step e a msg
get generator =
  \_ _ tagger ->
    generator <| tagger << Ok


do : Cmd msg -> Step Never () msg
do command =
  \procId msgTagger resultTagger ->
    Task.succeed ()
      |> Task.perform (\_ ->
        let
          nextCommand =
            Task.succeed ()
              |> Task.perform (resultTagger << Ok)
        in
          Cmd.batch [ command, nextCommand ]
            |> Execute procId
            |> msgTagger
      )


waitFor : ((a -> msg) -> Sub msg) -> Step e a msg
waitFor =
  waitForValue (\_ -> True)


waitForValue : (a -> Bool) -> ((a -> msg) -> Sub msg) -> Step e a msg
waitForValue predicate generator =
  \procId msgTagger resultTagger ->
    generator (
      \aData ->
        if predicate aData then
          resultTagger <| Ok aData
        else
          msgTagger Continue
    )
      |> Task.succeed
      |> Task.perform (msgTagger << Subscribe procId)


send : a -> Step e a msg
send value =
  get <|
    \tagger ->
      Task.succeed value
        |> Task.perform tagger


break : e -> Step e a msg
break value =
  \_ _ tagger ->
    Task.succeed value
      |> Task.perform (tagger << Err)


catch : (e -> Step f a msg) -> Step e a msg -> Step f a msg
catch stepGenerator step =
  next step <|
    \aResult ->
      case aResult of
        Ok aData ->
          send aData
        Err eData ->
          stepGenerator eData


andThen : (a -> Step e b msg) -> Step e a msg -> Step e b msg
andThen stepGenerator step =
  next step <|
    \aResult ->
      case aResult of
        Ok aData ->
          stepGenerator aData
        Err eData ->
          break eData


sequence : List (Step e a msg) -> Step e (List a) msg
sequence steps =
  case steps of
    [] ->
      emptyStep
    step :: remainingSteps ->
      List.foldl (andThen << addToList) (addToList step []) remainingSteps


addToList : Step e a msg -> List a -> Step e (List a) msg
addToList step collector =
  next step <|
    \aResult ->
      case aResult of
        Ok aData ->
          aData :: []
            |> List.append collector
            |> send
        Err eData ->
          break eData


emptyStep : Step e a msg
emptyStep _ _ _ =
  Cmd.none


map : (a -> b) -> Step e a msg -> Step e b msg
map mapper =
  andThen (send << mapper)


mapError : (e -> f) -> Step e a msg -> Step f a msg
mapError mapper step =
  next step <|
    \aResult ->
      case aResult of
        Ok aData ->
          send aData
        Err eData ->
          mapper eData
            |> break


next : Step e a msg -> (Result e a -> Step f b msg) -> Step f b msg
next step resultMapper =
  \procId msgTagger tagger ->
    step procId msgTagger <|
      \aResult ->
        (resultMapper aResult) procId msgTagger tagger
          |> msgTagger << Execute procId


try : (Msg msg -> msg) -> (Result e a -> msg) -> Step e a msg -> Cmd msg
try msgTagger tagger step =
  Task.succeed (\procId -> step procId msgTagger tagger)
    |> Task.perform (msgTagger << Initiate)


run : (Msg msg -> msg) -> (a -> msg) -> Step Never a msg -> Cmd msg
run msgTagger tagger step =
  try msgTagger (\result ->
    case result of
      Ok data ->
        tagger data
      Err e ->
        never e
  ) step


-----


type alias ProcedureId =
  Int


type alias Model msg =
  { nextId: ProcedureId
  , procedures: Dict ProcedureId (ProcedureModel msg)
  }


defaultModel : Model msg
defaultModel =
  { nextId = 0
  , procedures = Dict.empty
  }


type alias ProcedureModel msg =
  { subscriptions: Sub msg
  }


procedureModel : Sub msg -> ProcedureModel msg
procedureModel sub =
  { subscriptions = sub
  }


type Msg msg
  = Initiate (ProcedureId -> Cmd msg)
  | Execute ProcedureId (Cmd msg)
  | Subscribe ProcedureId (Sub msg)
  | Continue


update : Msg msg -> Model msg -> (Model msg, Cmd msg)
update msg model =
  case msg of
    Initiate generator ->
      ( { model | nextId = model.nextId + 1 }
      , generator model.nextId
      )
    Execute procedureId cmd ->
      ( { model | procedures = Dict.remove procedureId model.procedures }
      , cmd
      )
    Subscribe procedureId sub ->
      ( { model | procedures = Dict.insert procedureId (procedureModel sub) model.procedures }
      , Cmd.none
      )
    Continue ->
      ( model, Cmd.none )


subscriptions : Model msg -> Sub msg
subscriptions model =
  Dict.values model.procedures
    |> List.map .subscriptions
    |> Sub.batch
