module TestHelpers exposing
  ( Msg(..)
  , procedureCommandTestState
  , expectValue
  , expectError
  , stringCommand
  , intCommand
  )

import Expect
import Elmer exposing (TestState)
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Command as Command
import Html exposing (Html)


procedureCommandTestState : TestState Model Msg
procedureCommandTestState =
  Elmer.given testModel emptyView testUpdate
    |> Spy.use [ stringCommandSpy, intCommandSpy ]


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
  = CmdTagger (Cmd Msg)
  | TestStringTagger String
  | TestResultTagger (Result String String)


type alias Model =
  { message : String
  , error : String
  }


testModel =
  { message = ""
  , error = ""
  }


testUpdate : Msg -> Model -> (Model, Cmd Msg)
testUpdate msg model =
  case msg of
    CmdTagger cmd ->
      (model, cmd)
    TestStringTagger value ->
      ({ model | message = value }, Cmd.none)
    TestResultTagger value ->
      case value of
        Ok data ->
          ({ model | message = data }, Cmd.none)
        Err data ->
          ({ model | error = data }, Cmd.none)


emptyView : Model -> Html Msg
emptyView _ =
  Html.text ""