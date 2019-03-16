module TestHelpers exposing
  ( Msg(..)
  , procedureCommandTestState
  , expectValue
  , expectError
  , stringCommand
  , stringSubscription
  , testSubscriptions
  , intCommand
  )

import Expect
import Elmer exposing (TestState)
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Command as Command
import Elmer.Subscription as Subscription
import Html exposing (Html)
import Procedure


procedureCommandTestState : TestState Model Msg
procedureCommandTestState =
  Elmer.given testModel emptyView testUpdate
    |> Spy.use [ stringCommandSpy, intCommandSpy, stringSubscriptionSpy ]


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


stringCommandSpy : Spy
stringCommandSpy =
  Spy.observe (\_ -> stringCommand)
    |> andCallFake (\word tagger -> 
      tagger word |> Command.fake
    )


stringCommand : String -> (String -> Msg) -> Cmd Msg
stringCommand _ _ =
  Cmd.none


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


intCommandSpy : Spy
intCommandSpy =
  Spy.observe (\_ -> intCommand)
    |> andCallFake (\number tagger -> 
      tagger number |> Command.fake
    )


intCommand : Int -> (Int -> Msg) -> Cmd Msg
intCommand _ _ =
  Cmd.none


type Msg
  = ProcedureTagger (Procedure.Msg Msg)
  | TestStringTagger String
  | TestResultTagger (Result String String)


type alias Model =
  { procedureModel : Procedure.Model Msg
  , message : String
  , error : String
  }


testModel =
  { procedureModel = Procedure.defaultModel
  , message = ""
  , error = ""
  }


testUpdate : Msg -> Model -> (Model, Cmd Msg)
testUpdate msg model =
  case msg of
    ProcedureTagger pMsg ->
      Procedure.update pMsg model.procedureModel
        |> Tuple.mapFirst (\updatedModel -> { model | procedureModel = updatedModel })
    TestStringTagger value ->
      ({ model | message = value }, Cmd.none)
    TestResultTagger value ->
      case value of
        Ok data ->
          ({ model | message = data }, Cmd.none)
        Err data ->
          ({ model | error = data }, Cmd.none)


testSubscriptions : Model -> Sub Msg
testSubscriptions model =
  Procedure.subscriptions model.procedureModel


emptyView : Model -> Html Msg
emptyView _ =
  Html.text ""