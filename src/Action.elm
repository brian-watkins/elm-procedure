module Action exposing
  ( Action
  , with
  , andThen
  , perform
  )


type alias Action a msg =
  (Cmd msg -> msg) -> (a -> msg) -> Cmd msg


with : ((a -> msg) -> Cmd msg) -> Action a msg
with generator =
  \_ tagger ->
    generator tagger


andThen : (a -> ((b -> msg) -> Cmd msg)) -> Action a msg -> Action b msg
andThen mapper action =
  \cmdTagger bTagger -> 
    action cmdTagger <|
      \aData -> 
        bTagger 
          |> mapper aData
          |> cmdTagger


perform : (Cmd msg -> msg) -> (a -> msg) -> Action a msg -> Cmd msg
perform cmdTagger tagger action =
  action cmdTagger tagger
