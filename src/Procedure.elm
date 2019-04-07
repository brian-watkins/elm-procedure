module Procedure exposing
  ( Step
  , do, fetch, provide, collect, fromTask, break
  , catch, andThen
  , map, map2, map3, mapError
  , try, run
  )

{-| Orchestrate commands, subscriptions, and tasks.

@docs Step

# Execute a Procedure
@docs run, try

# Basic Steps
@docs provide, fetch, collect, fromTask, break, do

# Build a Procedure
@docs andThen, catch

# Map a Step
@docs map, map2, map3, mapError

-}

import Task exposing (Task)
import Procedure.Internal exposing (Step(..), Msg(..))


{-| Represents a step in a procedure.
-}
type alias Step e a msg =
  Procedure.Internal.Step e a msg


{-| Generate a step that executes a `Cmd` with a callback function.

For example, if you wanted to have the user select a file and then convert
that to a string, you could do the following:

    Procedure.fetch (File.Select.file ["application/zip"])
      |> Procedure.andThen (\file -> 
        Procedure.fromTask <| File.toString file
      )
      |> Procedure.run ProcedureTagger StringTagger

-}
fetch : ((a -> msg) -> Cmd msg) -> Step e a msg
fetch generator =
  Step <| 
    \_ _ tagger ->
      generator <| tagger << Ok


{-| Generate a step that executes a `Cmd` without a callback.

Use `do` to execute port functions that generate `Cmd` values. 

    Procedure.do myFunPortCommand
      |> Procedure.map (\_ -> "We did it!")
      |> Procedure.run ProcedureTagger StringTagger

If you want to send a port command and expect a response via some subscription, use
a `Channel`.

-}
do : Cmd msg -> Step Never () msg
do command =
  Step <| 
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


{-| Generate a step that simply provides a value.

    Procedure.send "Hello!"
      |> Procedure.run ProcedureTagger StringTagger

This will result in `StringTagger "Hello"`.

-}
provide : a -> Step e a msg
provide value =
  fetch <|
    \tagger ->
      Task.succeed value
        |> Task.perform tagger


{-| Generate a step that runs a task.

For example, if you wanted to have the user select a file and then convert
that to a string, you could do the following:

    Procedure.fetch (File.Select.file ["application/zip"])
      |> Procedure.andThen (\file -> 
        Procedure.fromTask <| File.toString file
      )
      |> Procedure.run ProcedureTagger StringTagger

If the task fails, the procedure will break at this point, just as
if `Procedure.break` had been used.

-}
fromTask : Task e a -> Step e a msg
fromTask task =
  Step <|
    \procId msgTagger resultTagger ->
      Task.attempt resultTagger task


{-| Generate a step that breaks out of the current procedure.

You can use this to stop a procedure early:

    Procedure.fetch (File.Select.file ["text/plain"])
      |> Procedure.andThen (\file -> 
        Procedure.fromTask <| File.toString file
      )
      |> Procedure.andThen (\text ->
        if String.length text > 100 then
          Procedure.break "File is too long!"
        else
          Procedure.provide text
      )
      |> Procedure.map (\text -> doSomethingWithTheText text)
      |> Procedure.try ProcedureTagger StringResultTagger

where `StringResultTagger` tags a `Result String String`. If the break is triggered,
then the result would be `Err "File is too long!"`.

-}
break : e -> Step e a msg
break value =
  Step <| 
    \_ _ tagger ->
      Task.succeed value
        |> Task.perform (tagger << Err)


{-| Generate a new step when some previous step results in an error, usually
do to processing a `break` step.

For example, you could check the result of some data
and then break to skip the next steps until you reach a `catch` step.

    Procedure.fetch someCommand
      |> Procedure.andThen (\data ->
        if data.message == "Success" then
          Procedure.provide data
        else
          Procedure.break data.message
      )
      |> Procedure.andThen (\data ->
        Channel.send (somePortCommand data)
          |> Channel.receive somePortSubscription
          |> Channel.await
      )
      |> Procedure.catch (\errorData -> 
        Procedure.provide "Some default message"
      )
      |> Procedure.map (\data -> data ++ "!")
      |> Procedure.run ProcedureTagger StringTagger

If the data fetched via `someCommand` is deemed not successful, then 
the next steps will be skipped and the result of this procedure will
be `StringTagger "Some default message!"`.

-}
catch : (e -> Step f a msg) -> Step e a msg -> Step f a msg
catch stepGenerator step =
  next step <|
    \aResult ->
      case aResult of
        Ok aData ->
          provide aData
        Err eData ->
          stepGenerator eData


{-| Generate a new step based on the result of the previous step.

    Procedure.provide "An awesome value"
      |> Procedure.andThen (\data -> 
        Procedure.provide <| data ++ "!!!"
      )
      |> Procedure.run ProcedureTagger StringTagger

Then the result would be `StringTagger "An awesome value!!!"`.

-}
andThen : (a -> Step e b msg) -> Step e a msg -> Step e b msg
andThen stepGenerator step =
  next step <|
    \aResult ->
      case aResult of
        Ok aData ->
          stepGenerator aData
        Err eData ->
          break eData


{-| Generate a step that collects the results of a list of steps.

    Procedure.collect
      [ Procedure.provide "One"
      , Procedure.provide "Two"
      , Procedure.provide "Three"
      ]
      |> Procedure.run ProcedureTagger ListStringTagger

Then the result would be `ListStringTagger [ "One", "Two", "Three" ]`.

-}
collect : List (Step e a msg) -> Step e (List a) msg
collect steps =
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
            |> provide
        Err eData ->
          break eData


emptyStep : Step e a msg
emptyStep =
  Step <|
    \_ _ _ -> Cmd.none


{-| Generate a step that transforms the value of the previous step.

    Procedure.collect
      [ Procedure.provide "One"
      , Procedure.provide "Two"
      , Procedure.provide "Three"
      ]
      |> Procedure.map (String.join ", ")
      |> Procedure.run ProcedureTagger StringTagger

Then the result would be `StringTagger "One, Two, Three"`.
-}
map : (a -> b) -> Step e a msg -> Step e b msg
map mapper =
  andThen (provide << mapper)


{-| Generate a step that provides a new value based on the values of two other steps

    Procedure.map2
      (\a b -> a ++ " AND " ++ b)
      (Procedure.provide "One")
      (Procedure.provide "Two")
      |> Procedure.run ProcedureTagger StringTagger

Then the result would be `StringTagger "One AND Two"
-}
map2 : (a -> b -> c) -> Step e a msg -> Step e b msg -> Step e c msg
map2 mapper stepA stepB =
  stepA
    |> andThen (\aData ->
      stepB
        |> map (mapper aData)
    )


{-| Generate a step that provides a new value based on the values of three other steps.

-}
map3 : (a -> b -> c -> d) -> Step e a msg -> Step e b msg -> Step e c msg -> Step e d msg
map3 mapper stepA stepB stepC =
  stepA
    |> andThen (\aData ->
      map2 (mapper aData) stepB stepC
    )


{-| Generate a step that transforms the error value of a previous step.

    Procedure.provide "Start"
      |> Procedure.andThen (\_ -> Procedure.break "Oops")
      |> Procedure.mapError (\err -> err ++ "???")
      |> Procedure.try ProcedureTagger ResultTagger

Then the result would be `Err "Oops???"`.

Note: Error values can be set explicitly by using `break`. 
-}
mapError : (e -> f) -> Step e a msg -> Step f a msg
mapError mapper step =
  next step <|
    \aResult ->
      case aResult of
        Ok aData ->
          provide aData
        Err eData ->
          mapper eData
            |> break


next : Step e a msg -> (Result e a -> Step f b msg) -> Step f b msg
next (Step step) resultMapper =
  Step <| 
    \procId msgTagger tagger ->
      step procId msgTagger <|
        \aResult ->
          let
            (Step nextStep) =
              resultMapper aResult
          in
            nextStep procId msgTagger tagger
              |> msgTagger << Execute procId


{-| Execute a procedure that may fail. 
-}
try : (Msg msg -> msg) -> (Result e a -> msg) -> Step e a msg -> Cmd msg
try msgTagger tagger (Step step) =
  Task.succeed (\procId -> step procId msgTagger tagger)
    |> Task.perform (msgTagger << Initiate)


{-| Execute a procedure that cannot fail
-}
run : (Msg msg -> msg) -> (a -> msg) -> Step Never a msg -> Cmd msg
run msgTagger tagger step =
  try msgTagger (\result ->
    case result of
      Ok data ->
        tagger data
      Err e ->
        never e
  ) step
