port module TestHelpers exposing
  ( Msg(..)
  , procedureCommandTestState
  , expectValue
  , expectError
  , expectUnit
  , stringCommand
  , stringPortCommand
  , stringSubscription
  , keySubscription
  , testSubscriptions
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
import Procedure
import Procedure.Config


procedureCommandTestState : TestState Model Msg
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

expectValue : String -> TestState Model Msg -> Expect.Expectation
expectValue expected testState =
  testState
    |> Elmer.expectModel (\model ->
        Expect.equal model.message expected
    )


expectError : String -> TestState Model Msg -> Expect.Expectation
expectError expected testState =
  testState
    |> Elmer.expectModel (\model ->
        Expect.equal model.error expected
    )


expectUnit : TestState Model Msg -> Expect.Expectation
expectUnit testState =
  testState
    |> Elmer.expectModel (\model ->
      Expect.true "Expected to receive a unit" model.didReceiveUnit
    )


stringCommandSpy : Spy
stringCommandSpy =
  Spy.observe (\_ -> stringCommand)
    |> andCallFake (\word tagger -> 
      tagger word |> Command.fake
    )


stringCommand : String -> (String -> Msg) -> Cmd Msg
stringCommand _ _ =
  Cmd.none


port stringPortCommand : String -> Cmd msg


stringPortCommandSpy : Spy
stringPortCommandSpy =
  Spy.observe (\_ -> stringPortCommand)
    |> andCallFake (\_ -> Command.dummy "string-port")


stringSubscription : String -> (String -> Msg) -> Sub Msg
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


keySubscription : (SubDescription -> Msg) -> Sub Msg
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


intCommand : Int -> (Int -> Msg) -> Cmd Msg
intCommand _ _ =
  Cmd.none


intSubscription : (Int -> Msg) -> Sub Msg
intSubscription _ =
  Sub.none


intSubscriptionSpy : Spy
intSubscriptionSpy =
  Spy.observe (\_ -> intSubscription)
    |> andCallFake (Subscription.fake "int-subscription")


type Msg
  = ProcedureTagger (Procedure.Config.Msg Msg)
  | TestStringTagger String
  | TestResultTagger (Result String String)
  | TestUnitTagger ()


type alias Model =
  { procedureModel : Procedure.Config.Model Msg
  , message : String
  , didReceiveUnit : Bool
  , error : String
  }


testModel =
  { procedureModel = Procedure.Config.init
  , message = ""
  , didReceiveUnit = False
  , error = ""
  }


testUpdate : Msg -> Model -> (Model, Cmd Msg)
testUpdate msg model =
  case msg of
    ProcedureTagger pMsg ->
      Procedure.Config.update pMsg model.procedureModel
        |> Tuple.mapFirst (\updatedModel -> { model | procedureModel = updatedModel })
    TestStringTagger value ->
      ( { model | message = value }, Cmd.none )
    TestResultTagger value ->
      case value of
        Ok data ->
          ( { model | message = data }, Cmd.none )
        Err data ->
          ( { model | error = data }, Cmd.none )
    TestUnitTagger _ ->
      ( { model | didReceiveUnit = True }, Cmd.none )


testSubscriptions : Model -> Sub Msg
testSubscriptions model =
  Procedure.Config.subscriptions model.procedureModel


emptyView : Model -> Html Msg
emptyView _ =
  Html.text ""