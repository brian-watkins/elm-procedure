port module TestHelpers exposing
  ( Msg(..)
  , runProcedure
  , andRunProcedure
  , tryProcedure
  , andTryProcedure
  , expect
  , expectValue
  , expectResult
  , stringCommand
  , stringPortCommand
  , stringSubscription
  , keySubscription
  , testSubscriptions
  , testSubscriptionsWithExtraSubs
  , intCommand
  , intSubscription
  )

import Expect
import Elmer exposing (TestState)
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Command as Command
import Elmer.Subscription as Subscription
import Html exposing (Html)
import Task
import Process
import Procedure exposing (Procedure)
import Procedure.Config


runProcedure : (() -> Procedure Never a (Msg e a)) -> TestState (Model e a) (Msg e a)
runProcedure procThunk =
  procedureCommandTestState
    |> andRunProcedure procThunk

andRunProcedure : (() -> Procedure Never a (Msg e a)) -> TestState (Model e a) (Msg e a) -> TestState (Model e a) (Msg e a)
andRunProcedure procThunk testState =
  testState
    |> Command.send (\_ ->
      procThunk ()
        |> Procedure.run ProcedureTagger TestValueTagger
    )

tryProcedure : (() -> Procedure e a (Msg e a)) -> TestState (Model e a) (Msg e a)
tryProcedure procThunk =
  procedureCommandTestState
    |> andTryProcedure procThunk

andTryProcedure : (() -> Procedure e a (Msg e a)) -> TestState (Model e a) (Msg e a) -> TestState (Model e a) (Msg e a)
andTryProcedure procThunk testState =
  testState
    |> Command.send (\_ ->
      procThunk ()
        |> Procedure.try ProcedureTagger TestResultTagger
    )

procedureCommandTestState : TestState (Model e a) (Msg e a)
procedureCommandTestState =
  Elmer.given testModel emptyView testUpdate
    |> Spy.use
      [ stringCommandSpy
      , intCommandSpy
      , intSubscriptionSpy
      , stringSubscriptionSpy
      , keySubscriptionSpy
      , stringPortCommandSpy
      , processSpy
      ]


processSpy : Spy
processSpy =
  Spy.observe (\_ -> Process.sleep)
    |> andCallFake (\timeout ->
      Task.succeed ()
    )

expectValue : a -> TestState (Model e a) (Msg e a) -> Expect.Expectation
expectValue expectedValue =
  expect (\values ->
    case List.head values of
      Just value ->
        Expect.equal expectedValue value
      Nothing ->
        Expect.fail "The procedure produced no value!"
  )

expect : (List a -> Expect.Expectation) -> TestState (Model e a) (Msg e a) -> Expect.Expectation
expect handler testState =
  testState
    |> Elmer.expectModel (\model ->
        handler model.values
    )

expectResult : Result e a -> TestState (Model e a) (Msg e a) -> Expect.Expectation
expectResult expectedResult testState =
  testState
    |> Elmer.expectModel (\model ->
      case List.head model.results of
        Just result ->
          Expect.equal expectedResult result
        Nothing ->
          Expect.fail "The procedure produced no result!"
    )


stringCommandSpy : Spy
stringCommandSpy =
  Spy.observe (\_ -> stringCommand)
    |> andCallFake (\word tagger -> 
      tagger word |> Command.fake
    )


stringCommand : String -> (String -> Msg e a) -> Cmd (Msg e a)
stringCommand _ _ =
  Cmd.none


port stringPortCommand : String -> Cmd msg


stringPortCommandSpy : Spy
stringPortCommandSpy =
  Spy.observe (\_ -> stringPortCommand)
    |> andCallFake (\_ -> Command.dummy "string-port")


stringSubscription : String -> (String -> Msg e a) -> Sub (Msg e a)
stringSubscription _ _ =
  Sub.none


stringSubscriptionSpy : Spy
stringSubscriptionSpy =
  Spy.observe (\_ -> stringSubscription)
    |> andCallFake (\word tagger ->
      Subscription.fake "string-subscription" <|
        \val ->
          tagger <| word ++ " then " ++ val
    )


type alias SubDescription =
  { key: String
  , value: String
  }


keySubscription : (SubDescription -> Msg e a) -> Sub (Msg e a)
keySubscription _ =
  Sub.none


keySubscriptionSpy : Spy
keySubscriptionSpy =
  Spy.observe (\_ -> keySubscription)
    |> andCallFake (Subscription.fake "key-subscription")


intCommandSpy : Spy
intCommandSpy =
  Spy.observe (\_ -> intCommand)
    |> andCallFake (\number tagger -> 
      tagger number |> Command.fake
    )


intCommand : Int -> (Int -> Msg e a) -> Cmd (Msg e a)
intCommand _ _ =
  Cmd.none


intSubscription : (Int -> Msg e a) -> Sub (Msg e a)
intSubscription _ =
  Sub.none


intSubscriptionSpy : Spy
intSubscriptionSpy =
  Spy.observe (\_ -> intSubscription)
    |> andCallFake (Subscription.fake "int-subscription")


type Msg e a
  = ProcedureTagger (Procedure.Config.Msg (Msg e a))
  | TestValueTagger a
  | TestResultTagger (Result e a)
  | UnusedIntSubTagger Int
  | UnusedStringSubTagger String


type alias Model e a =
  { procedureModel : Procedure.Config.Model (Msg e a)
  , values : List a
  , results : List (Result e a)
  }


testModel =
  { procedureModel = Procedure.Config.init
  , values = []
  , results = []
  }


testUpdate : Msg e a -> Model e a -> (Model e a, Cmd (Msg e a))
testUpdate msg model =
  case msg of
    ProcedureTagger pMsg ->
      Procedure.Config.update pMsg model.procedureModel
        |> Tuple.mapFirst (\updatedModel -> { model | procedureModel = updatedModel })
    TestValueTagger value ->
      ( { model | values = value :: model.values }, Cmd.none )
    TestResultTagger result ->
      ( { model | results = result :: model.results }, Cmd.none )
    UnusedIntSubTagger _ ->
      ( model, Cmd.none )
    UnusedStringSubTagger _ ->
      ( model, Cmd.none )


testSubscriptions : Model e a -> Sub (Msg e a)
testSubscriptions model =
  Procedure.Config.subscriptions model.procedureModel


testSubscriptionsWithExtraSubs : Model e a -> Sub (Msg e a)
testSubscriptionsWithExtraSubs model =
  Sub.batch
  [ Procedure.Config.subscriptions model.procedureModel
  , intSubscription UnusedIntSubTagger
  , stringSubscription "unused" UnusedStringSubTagger
  ]


emptyView : Model e a -> Html (Msg e a)
emptyView _ =
  Html.text ""