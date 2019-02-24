module Procedure exposing
  ( Step
  , first
  , andThen
  , perform
  )


type alias Step a msg =
  (Cmd msg -> msg) -> (a -> msg) -> Cmd msg


first : ((a -> msg) -> Cmd msg) -> Step a msg
first generator =
  \_ tagger ->
    generator tagger


andThen : (a -> ((b -> msg) -> Cmd msg)) -> Step a msg -> Step b msg
andThen mapper step =
  \cmdTagger bTagger -> 
    step cmdTagger <|
      \aData -> 
        bTagger 
          |> mapper aData
          |> cmdTagger


perform : (Cmd msg -> msg) -> (a -> msg) -> Step a msg -> Cmd msg
perform cmdTagger tagger step =
  step cmdTagger tagger
