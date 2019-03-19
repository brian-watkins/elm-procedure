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


type alias ProcedureId =
  Int

type alias Step e a msg =
  ProcedureId -> (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg


get : ((a -> msg) -> Cmd msg) -> Step e a msg
get generator =
  \_ tagger ->
    generator <| tagger << Ok


do : Cmd msg -> Step Never () msg
do command =
  \msgTagger resultTagger ->
    Task.succeed ()
      |> Task.perform (\_ ->
        let
          nextCommand =
            Task.succeed ()
              |> Task.perform (resultTagger << Ok)
        in
          Cmd.batch [ command, nextCommand ]
            |> CmdTagger
            |> msgTagger
      )


waitFor : ((a -> msg) -> Sub msg) -> Step e a msg
waitFor =
  waitForValue (\_ -> True)


waitForValue : (a -> Bool) -> ((a -> msg) -> Sub msg) -> Step e a msg
waitForValue predicate generator =
  \msgTagger resultTagger ->
    generator (
      \aData ->
        if predicate aData then
          resultTagger <| Ok aData
        else
          msgTagger Ignore
    )
      |> Task.succeed
      |> Task.perform (msgTagger << SubTagger)


send : a -> Step e a msg
send value =
  get <|
    \tagger ->
      Task.succeed value
        |> Task.perform tagger


break : e -> Step e a msg
break value =
  \_ tagger ->
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
          |> msgTagger << CmdTagger


-- here we would want to do something like 
-- pass in a function that takes a procedureId and generates the command
-- since the command is based on the step we are given
-- and send an allocate command

try : (Msg msg -> msg) -> (Result e a -> msg) -> Step e a msg -> Cmd msg
try pTagger tagger step =
  -- step pTagger tagger
  Task.succeed (\procedureId -> step procedureId pTagger tagger)
    |> Task.perform (pTagger << Allocate)


run : (Msg msg -> msg) -> (a -> msg) -> Step Never a msg -> Cmd msg
run pTagger tagger step =
  try pTagger (\result ->
    case result of
      Ok data ->
        tagger data
      Err e ->
        never e
  ) step


-----

type alias ExportedModel msg =
  { procedureId: Int
  , models: Dict Int (Model msg)
  }

type alias Model msg =
  { subscriptions : Sub msg
  }


defaultModel : Model msg
defaultModel =
  { subscriptions = Sub.none
  }


type Msg msg
  = CmdTagger (Cmd msg)
  | SubTagger ProcedureId (Sub msg)
  | Ignore
  | Allocate (ProcedureId -> Cmd msg)
  | Deallocate ProcedureId
  | SubComplete ProcedureId


update : Msg msg -> ExportedModel msg -> (ExportedModel msg, Cmd msg)
update msg model =
  case msg of
    CmdTagger cmd ->
      ( model, cmd )
    SubTagger procedureId sub ->
      let
        updatedModels = 
          Dict.insert procedureId { subscriptions = sub } model.models
      in
        ( { model | models = updatedModels }, Cmd.none )
    SubComplete procedureId ->
      let
        updatedModels = 
          Dict.insert procedureId defaultModel model.models
      in
        ( { model | models = updatedModels }, Cmd.none )
    Ignore ->
      ( model, Cmd.none )
    Allocate generator ->
      let
        nextId = model.procedureId + 1
        updatedModels = Dict.insert nextId defaultModel model.models
      in
        ( { model | procedureId = nextId, models = updatedModels }, generator nextId )
    Deallocate procedureId ->
      ( { model | models = Dict.remove procedureId model.models }, Cmd.none )

subscriptions : ExportedModel msg -> Sub msg
subscriptions model =
  Dict.values model.models
    |> List.map .subscriptions
    |> Sub.batch
  -- model.subscriptions


-- What do I want to achieve here?
-- Suppose you install one procedure model, tagger and then you want to be able to 
-- run as many distinct procedures as necessary. 
-- Some of these procedures could be running at the same time. 
-- So: Handle multiple distinct procedures, each conceivable waiting on different subscriptions
--     at the same time.
-- Also: Handle the case where the same procedure is triggered again before it has completed. Again, 
--       the problem here is only if that procedure has a subscription. 
--
-- Note that the model merely stores subscriptions. Would it be possible to have a distinct model
-- for each triggered procedure? 
-- It's possible to add a command at the beginning of each procedure, I guess, that goes and
-- updates our internal model with a new area for storing things pertaining to this procedure? and
-- then removes it at the end?
-- But how do I get the procedure id as part of the step?
-- Procedure.run and Procedure.try might need to take in the model and return it, kind of like the update
-- function ... that way they could create a new id and pass it to the steps ... ?
-- OR
-- Maybe we need to try to pass the tagger (as a function that takes a procedureId) 
-- when we send the Allocate message, then in the update function
-- we can execute that function with the procedureId to get the real tagger thereby passing in the id
-- and then just execute a command that uses that tagger? 