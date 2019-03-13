module Procedure exposing
  ( Step
  , do
  , andThen
  , map
  , sequence
  , perform
  )

import Task


type alias Step a msg =
  (Cmd msg -> msg) -> (a -> msg) -> Cmd msg


do : ((a -> msg) -> Cmd msg) -> Step a msg
do generator =
  \_ tagger ->
    generator tagger


andThen : (a -> Step b msg) -> Step a msg -> Step b msg
andThen stepGenerator step =
  \cmdTagger bTagger -> 
    step cmdTagger <|
      \aData -> 
        stepGenerator aData cmdTagger bTagger
          |> cmdTagger


sequence : List (Step a msg) -> Step (List a) msg
sequence steps =
  case steps of
    [] ->
      emptyStep
    step :: remainingSteps ->
      List.foldl (andThen << addToList) (addToList step []) remainingSteps


addToList : Step a msg -> List a -> Step (List a) msg
addToList step collector =
  \cmdTagger listTagger ->
    step cmdTagger <| \aData ->
      aData :: []
        |> List.append collector
        |> listTagger


emptyStep : Step a msg
emptyStep _ _ =
  Cmd.none


map : (a -> b) -> Step a msg -> Step b msg
map mapper step =
  step
    |> andThen (\aData _ tagger ->
        mapper aData
          |> Task.succeed
          |> Task.perform tagger
    )


perform : (Cmd msg -> msg) -> (a -> msg) -> Step a msg -> Cmd msg
perform cmdTagger tagger step =
  step cmdTagger tagger
