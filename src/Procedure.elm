module Procedure exposing
  ( Msg
  , Model
  , Step
  , defaultModel
  , do
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


type alias Step e a msg =
  (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg


do : ((a -> msg) -> Cmd msg) -> Step e a msg
do generator =
  \_ tagger ->
    generator <| tagger << Ok


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
  do <|
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
emptyStep _ _ =
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
  \msgTagger tagger ->
    step msgTagger <|
      \aResult ->
        (resultMapper aResult) msgTagger tagger
          |> msgTagger << CmdTagger


try : (Msg msg -> msg) -> (Result e a -> msg) -> Step e a msg -> Cmd msg
try pTagger tagger step =
  step pTagger tagger


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

type alias Model msg =
  { subscriptions : Sub msg
  }


defaultModel : Model msg
defaultModel =
  { subscriptions = Sub.none
  }


type Msg msg
  = CmdTagger (Cmd msg)
  | SubTagger (Sub msg)
  | Ignore


update : Msg msg -> Model msg -> (Model msg, Cmd msg)
update msg model =
  case msg of
    CmdTagger cmd ->
      ( { model | subscriptions = Sub.none }, cmd )
    SubTagger sub ->
      ( { model | subscriptions = sub }, Cmd.none )
    Ignore ->
      ( model, Cmd.none )


subscriptions : Model msg -> Sub msg
subscriptions model =
  model.subscriptions
