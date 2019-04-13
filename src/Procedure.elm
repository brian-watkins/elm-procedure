module Procedure exposing
  ( Procedure
  , do, fetch, provide, collect, fromTask, break
  , catch, andThen
  , map, map2, map3, mapError
  , try, run
  )

{-| Orchestrate commands, subscriptions, and tasks.

@docs Procedure

# Execute a Procedure
@docs run, try

# Basic Procedures
@docs provide, fetch, collect, fromTask, break, do

# Build a Procedure
@docs andThen, catch

# Map the Output of a Procedure
@docs map, map2, map3, mapError

-}

import Task exposing (Task)
import Procedure.Internal exposing (Procedure(..), Msg(..))


{-| Represents some sequence of commands, subscriptions, or tasks that ultimately results in some value. 
-}
type alias Procedure e a msg =
  Procedure.Internal.Procedure e a msg


{-| Generate a procedure that gets the value produced by executing some `Cmd`.

For example, if you wanted to have the user select a file and then convert
that to a string, you could do the following:

    Procedure.fetch (File.Select.file ["application/zip"])
      |> Procedure.andThen (\file -> 
        File.toString file
          |> Procedure.fromTask
      )
      |> Procedure.run ProcedureTagger StringTagger

Note that only some commands produce a value directly. To execute those commands
that do not produce a value, use `do`. 

-}
fetch : ((a -> msg) -> Cmd msg) -> Procedure e a msg
fetch generator =
  Procedure <| 
    \_ _ tagger ->
      generator <| tagger << Ok


{-| Generate a procedure that executes a `Cmd` that does not produce any value directly.

Use `do` to execute port functions that generate `Cmd` values. 

    Procedure.do myFunPortCommand
      |> Procedure.map (\_ -> "We did it!")
      |> Procedure.run ProcedureTagger StringTagger

If you want to send a port command and expect a response later via some subscription, use
a `Channel`.

-}
do : Cmd msg -> Procedure Never () msg
do command =
  Procedure <| 
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


{-| Generate a procedure that simply provides a value.

    Procedure.send "Hello!"
      |> Procedure.run ProcedureTagger StringTagger

This will result in `StringTagger "Hello"`.

-}
provide : a -> Procedure e a msg
provide =
  fromTask << Task.succeed


{-| Generate a procedure that runs a task.

For example, if you wanted to have the user select a file and then convert
that to a string, you could do the following:

    Procedure.fetch (File.Select.file ["application/zip"])
      |> Procedure.andThen (\file ->
        File.toString file
          |> Procedure.fromTask
      )
      |> Procedure.run ProcedureTagger StringTagger

If the task fails, the procedure will break at this point, just as
if `Procedure.break` had been used.

-}
fromTask : Task e a -> Procedure e a msg
fromTask task =
  Procedure <|
    \procId msgTagger resultTagger ->
      Task.attempt resultTagger task


{-| Generate a procedure that breaks out of the current procedure.

You can use this to stop a procedure early:

    Procedure.fetch (File.Select.file ["text/plain"])
      |> Procedure.andThen (\file ->
        File.toString file
          |> Procedure.fromTask
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
break : e -> Procedure e a msg
break =
  fromTask << Task.fail


{-| Generate a new procedure when some previous procedure results in an error, usually
do to processing a `break` procedure.

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
catch : (e -> Procedure f a msg) -> Procedure e a msg -> Procedure f a msg
catch generator procedure =
  next procedure <|
    \aResult ->
      case aResult of
        Ok aData ->
          provide aData
        Err eData ->
          generator eData


{-| Generate a new procedure based on the result of the previous procedure.

    Procedure.provide "An awesome value"
      |> Procedure.andThen (\data -> 
        Procedure.provide <| data ++ "!!!"
      )
      |> Procedure.run ProcedureTagger StringTagger

Then the result would be `StringTagger "An awesome value!!!"`.

-}
andThen : (a -> Procedure e b msg) -> Procedure e a msg -> Procedure e b msg
andThen generator procedure =
  next procedure <|
    \aResult ->
      case aResult of
        Ok aData ->
          generator aData
        Err eData ->
          break eData


{-| Generate a procedure that collects the results of a list of procedures.

    Procedure.collect
      [ Procedure.provide "One"
      , Procedure.provide "Two"
      , Procedure.provide "Three"
      ]
      |> Procedure.run ProcedureTagger ListStringTagger

Then the result would be `ListStringTagger [ "One", "Two", "Three" ]`.

-}
collect : List (Procedure e a msg) -> Procedure e (List a) msg
collect procedures =
  case procedures of
    [] ->
      emptyProcedure
    procedure :: remainingProcedures ->
      List.foldl (andThen << addToList) (addToList procedure []) remainingProcedures


addToList : Procedure e a msg -> List a -> Procedure e (List a) msg
addToList procedure collector =
  next procedure <|
    \aResult ->
      case aResult of
        Ok aData ->
          aData :: []
            |> List.append collector
            |> provide
        Err eData ->
          break eData


emptyProcedure : Procedure e a msg
emptyProcedure =
  Procedure <|
    \_ _ _ -> Cmd.none


{-| Generate a procedure that transforms the value of the previous procedure.

    Procedure.collect
      [ Procedure.provide "One"
      , Procedure.provide "Two"
      , Procedure.provide "Three"
      ]
      |> Procedure.map (String.join ", ")
      |> Procedure.run ProcedureTagger StringTagger

Then the result would be `StringTagger "One, Two, Three"`.
-}
map : (a -> b) -> Procedure e a msg -> Procedure e b msg
map mapper =
  andThen (provide << mapper)


{-| Generate a procedure that provides a new value based on the values of two other procedures

    Procedure.map2
      (\a b -> a ++ " AND " ++ b)
      (Procedure.provide "One")
      (Procedure.provide "Two")
      |> Procedure.run ProcedureTagger StringTagger

Then the result would be `StringTagger "One AND Two"
-}
map2 : (a -> b -> c) -> Procedure e a msg -> Procedure e b msg -> Procedure e c msg
map2 mapper procedureA procedureB =
  procedureA
    |> andThen (\aData ->
      procedureB
        |> map (mapper aData)
    )


{-| Generate a procedure that provides a new value based on the values of three other procedures.

-}
map3 : (a -> b -> c -> d) -> Procedure e a msg -> Procedure e b msg -> Procedure e c msg -> Procedure e d msg
map3 mapper procedureA procedureB procedureC =
  procedureA
    |> andThen (\aData ->
      map2 (mapper aData) procedureB procedureC
    )


{-| Generate a procedure that transforms the error value of a previous procedure.

    Procedure.provide "Start"
      |> Procedure.andThen (\_ -> Procedure.break "Oops")
      |> Procedure.mapError (\err -> err ++ "???")
      |> Procedure.try ProcedureTagger ResultTagger

Then the result would be `Err "Oops???"`.

Note: Error values can be set explicitly by using `break`. 
-}
mapError : (e -> f) -> Procedure e a msg -> Procedure f a msg
mapError mapper procedure =
  next procedure <|
    \aResult ->
      case aResult of
        Ok aData ->
          provide aData
        Err eData ->
          mapper eData
            |> break


next : Procedure e a msg -> (Result e a -> Procedure f b msg) -> Procedure f b msg
next (Procedure procedure) resultMapper =
  Procedure <| 
    \procId msgTagger tagger ->
      procedure procId msgTagger <|
        \aResult ->
          let
            (Procedure nextProcedure) =
              resultMapper aResult
          in
            nextProcedure procId msgTagger tagger
              |> msgTagger << Execute procId


{-| Execute a procedure that may fail. 
-}
try : (Msg msg -> msg) -> (Result e a -> msg) -> Procedure e a msg -> Cmd msg
try msgTagger tagger (Procedure procedure) =
  Task.succeed (\procId -> procedure procId msgTagger tagger)
    |> Task.perform (msgTagger << Initiate)


{-| Execute a procedure that cannot fail.
-}
run : (Msg msg -> msg) -> (a -> msg) -> Procedure Never a msg -> Cmd msg
run msgTagger tagger =
  try msgTagger (\result ->
    case result of
      Ok data ->
        tagger data
      Err e ->
        never e
  )
