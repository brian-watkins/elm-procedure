module Procedure exposing
  ( Step
  , do
  , send
  , break
  , catch
  , andThen
  , map
  , mapError
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
  \cmdTagger tagger ->
    step cmdTagger <|
      \aResult ->
        (resultMapper aResult) cmdTagger tagger
          |> cmdTagger


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