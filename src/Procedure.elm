module Procedure exposing
  ( Step
  , do
  , send
  , break
  , andThen
  , map
  , sequence
  , try
  , run
  )

import Task


type alias Step e a msg =
  (Cmd msg -> msg) -> (Result e a -> msg) -> Cmd msg


do : ((a -> msg) -> Cmd msg) -> Step e a msg
do generator =
  \_ tagger ->
    generator <|
      \aData ->
        Ok aData
          |> tagger


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
      |> Task.perform (\eData -> 
        Err eData
          |> tagger
      )


andThen : (a -> Step e b msg) -> Step e a msg -> Step e b msg
andThen stepGenerator step =
  \cmdTagger bTagger ->
    step cmdTagger <|
      \aResult ->
        case aResult of
          Ok aData ->
            stepGenerator aData cmdTagger bTagger
              |> cmdTagger
          Err eData ->
            break eData cmdTagger bTagger
              |> cmdTagger


sequence : List (Step e a msg) -> Step e (List a) msg
sequence steps =
  case steps of
    [] ->
      emptyStep
    step :: remainingSteps ->
      List.foldl (andThen << addToList) (addToList step []) remainingSteps


addToList : Step e a msg -> List a -> Step e (List a) msg
addToList step collector =
  \cmdTagger listTagger ->
    step cmdTagger <| \aResult ->
      case aResult of
        Ok aData ->
          aData :: []
            |> List.append collector
            |> Ok
            |> listTagger
        Err eData ->
          break eData cmdTagger listTagger
            |> cmdTagger


emptyStep : Step e a msg
emptyStep _ _ =
  Cmd.none


map : (a -> b) -> Step e a msg -> Step e b msg
map mapper =
  andThen (send << mapper)


try : (Cmd msg -> msg) -> (Result e a -> msg) -> Step e a msg -> Cmd msg
try cmdTagger tagger step =
  step cmdTagger tagger


run : (Cmd msg -> msg) -> (a -> msg) -> Step Never a msg -> Cmd msg
run cmdTagger tagger step =
  try cmdTagger (\result ->
    case result of
      Ok data ->
        tagger data
      Err e ->
        never e
  ) step