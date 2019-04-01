module Procedure exposing
  ( Msg
  , Model
  , init
  , update
  , subscriptions
  , ProcedureId
  , Step
  , do
  , fetch
  , provide
  , collect
  , wait
  , waitFor
  , break
  , catch
  , andThen
  , map, map2, map3
  , mapError
  , try
  , run
  )

{-| Chain together functions that generate `Cmd` values and `Sub` values. 

@docs ProcedureId, Step

# Execute a Procedure
@docs run, try

# Build a Procedure
@docs andThen, catch

# Basic Steps
@docs provide, do, fetch, collect, break

# Steps that Wait
@docs wait, waitFor

# Map a Step
@docs map, map2, map3, mapError

# Use a Procedure
@docs Msg, Model, init, update, subscriptions

-}

import Task
import Dict exposing (Dict)


{-| Represents the unique identifier assigned to each procedure.

This is most useful in `do` and `waitFor` when you might pass the identifier
through a port to find a particular subscription. 
-}
type alias ProcedureId =
  Int


{-| Represents a step in a procedure.
-}
type alias Step e a msg =
  ProcedureId -> (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg


{-| Generate a step that executes a `Cmd` with a callback function.

For example, if you wanted to have the user select a file and then convert
that to a string, you could do the following:

    Procedure.fetch (File.Select.file ["application/zip"])
      |> Procedure.andThen (\file -> 
        File.toString file
          |> Task.perform
          |> Procedure.fetch
      )
      |> Procedure.run ProcedureTagger StringTagger

-}
fetch : ((a -> msg) -> Cmd msg) -> Step e a msg
fetch generator =
  \_ _ tagger ->
    generator <| tagger << Ok


{-| Generate a step that executes a `Cmd` without a callback.

This is mainly useful for executing port functions that generate `Cmd` values. 
Usually, this would be used in conjunction with wait like so:

    Procedure.do (\_ -> myFunPort)
      |> Procedure.andThen (Procedure.wait myFunSubscription)
      |> Procedure.run ProcedureTagger SubResultTagger

The `ProcedureId` is available when calling the `Cmd`-generating function. See
`waitFor` for an example of when that might come in handy.
-}
do : (ProcedureId -> Cmd msg) -> Step Never () msg
do generator =
  \procId msgTagger resultTagger ->
    Task.succeed ()
      |> Task.perform (\_ ->
        let
          nextCommand =
            Task.succeed ()
              |> Task.perform (resultTagger << Ok)
        in
          Cmd.batch [ generator procId, nextCommand ]
            |> Execute procId
            |> msgTagger
      )


{-| Generate a step that waits for a subscription.

Perhaps in 30 seconds you want to trigger some command:

    Procedure.wait (Time.every 30 * 1000)
      |> Procedure.andThen (Procedure.provide "My Message")
      |> Procedure.run ProcedureTagger StringTagger

Note: After a message is received by the subscription, it will no
longer be registered to accept messages until this procedure is
executed again.

-}
wait : ((a -> msg) -> Sub msg) -> Step e a msg
wait =
  waitFor (\_ _ -> True)


{-| Generate a step that waits for a particular value from a subscription.

Let's say you have two ports, one that generates a `Cmd` that makes a request
and one that generates a `Sub` that receives the response. You might worry that
a response you receive in the subscription might not match the request. 

To address this, you could use `do` to pass the `ProcedureId` through the port
and then pass it back in the subscription's data. Use `waitFor` to check the
data received to see if it contains the expected `ProcedureId`. 

    Procedure.do (\procId -> myPortRequest procId)
      |> Procedure.andThen (
        Procedure.waitFor
          (\procId data -> data.id == procId)
          (myPortResponse)
      )
      |> Procedure.run ProcedureTagger ResponseDataTagger

If the message received does not satisfy the predicate, then it will be ignored
by this procedure (but not by other procedures running independently).

Note: After a message is received by the subscription, it will no
longer be registered to accept messages until this procedure is
executed again.

-}
waitFor : (ProcedureId -> a -> Bool) -> ((a -> msg) -> Sub msg) -> Step e a msg
waitFor predicate generator =
  \procId msgTagger resultTagger ->
    generator (
      \aData ->
        if predicate procId aData then
          resultTagger <| Ok aData
        else
          msgTagger Continue
    )
      |> Task.succeed
      |> Task.perform (msgTagger << Subscribe procId)


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


{-| Generate a step that breaks out of the current procedure.

You can use this to stop a procedure early:

    Procedure.fetch (File.Select.file ["text/plain"])
      |> Procedure.andThen (\file -> 
        File.toString file
          |> Task.perform
          |> Procedure.fetch
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
  \_ _ tagger ->
    Task.succeed value
      |> Task.perform (tagger << Err)


{-| Generate a new step when a previous step is the result of a `break` step.

For example, you could check the result of some data you receive through a subscription
and then break to skip the next steps until you reach a `catch` step.

    Procedure.wait mySubscription
      |> Procedure.andThen (\data ->
        if data.message == "Success" then
          Procedure.provide data
        else
          Procedure.break data.message
      )
      |> Procedure.andThen (\data -> Procedure.do (\_ -> somePortRequest data))
      |> Procedure.andThen (\_ -> Procedure.wait somePortResponse)
      |> Procedure.catch (\errorData -> 
        Procedure.do (\_ -> Procedure.provide "Some default message")
      )
      |> Procedure.run ProcedureTagger StringTagger

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
      |> Procedure.andThen (\data -> Procedure.provide <| data ++ "!!!")
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
emptyStep _ _ _ =
  Cmd.none


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
next step resultMapper =
  \procId msgTagger tagger ->
    step procId msgTagger <|
      \aResult ->
        (resultMapper aResult) procId msgTagger tagger
          |> msgTagger << Execute procId


{-| Execute a procedure that may fail. 
-}
try : (Msg msg -> msg) -> (Result e a -> msg) -> Step e a msg -> Cmd msg
try msgTagger tagger step =
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


-----


{-| Represents the internal `Msg` values used to track the state of a procedure.

The type variable refers to the `Msg` type used by your application. 
You should provide a message type that wraps these values like so:

    type AppMsg
      = ProcMsg (Procedure.Msg AppMsg)

-}
type Msg msg
  = Initiate (ProcedureId -> Cmd msg)
  | Execute ProcedureId (Cmd msg)
  | Subscribe ProcedureId (Sub msg)
  | Continue


{-| Represents the internal state used to track running procedures. 

You should store this in your application's model like so:

    type alias AppModel =
      { procModel : Procedure.Model AppMsg
      }

-}
type Model msg =
  Model (Registry msg)


type alias Registry msg =
  { nextId: ProcedureId
  , procedures: Dict ProcedureId (ProcedureModel msg)
  }


{-| Generate the model used to track procedures.
-}
init : Model msg
init =
  Model <| 
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


{-| Update the state of running procedures.

You should add this to your application's update function like so:

    update : AppMsg -> AppModel -> (AppModel, Cmd AppMsg)
    update appMsg appModel =
      case appMsg of
        ProcedureTagger procMsg ->
          Procedure.update procMsg appModel.procModel
            |> Tuple.mapFirst (\updated -> { appModel | procModel = updated })

-}
update : Msg msg -> Model msg -> (Model msg, Cmd msg)
update msg (Model registry) =
  updateProcedures msg registry
    |> Tuple.mapFirst Model


updateProcedures : Msg msg -> Registry msg -> (Registry msg, Cmd msg)
updateProcedures msg registry =
  case msg of
    Initiate generator ->
      ( { registry | nextId = registry.nextId + 1 }
      , generator registry.nextId
      )
    Execute procedureId cmd ->
      ( { registry | procedures = Dict.remove procedureId registry.procedures }
      , cmd
      )
    Subscribe procedureId sub ->
      ( { registry | procedures = Dict.insert procedureId (procedureModel sub) registry.procedures }
      , Cmd.none
      )
    Continue ->
      ( registry, Cmd.none )


{-| Get any subscriptions necessary for running procedures. 

Add this to your application's subscriptions function like so:

    appSubscriptions : AppModel -> Sub AppMsg
    appSubscriptions appModel =
      Procedure.subscriptions appModel.procModel

-}
subscriptions : Model msg -> Sub msg
subscriptions (Model registry) =
  Dict.values registry.procedures
    |> List.map .subscriptions
    |> Sub.batch
