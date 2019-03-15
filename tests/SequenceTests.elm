module SequenceTests exposing (..)

import Expect
import Test exposing (..)
import Elmer exposing (TestState)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Html exposing (Html)
import Procedure


sequenceTests : Test
sequenceTests =
  describe "when a sequence of commands is executed"
  [ test "it collects the results in a list" <|
    \() ->
      Elmer.given testModel emptyView testUpdate
        |> Spy.use [ stringCommandSpy, intCommandSpy ]
        |> Command.send (\_ ->
            Procedure.sequence
              [ Procedure.do <| fakeStringCommand "Awesome"
              , Procedure.do <| fakeStringCommand "fun"
              , Procedure.do <| fakeStringCommand "stuff!!!"
              ]
              |> Procedure.map (\results -> String.join ", " results)
              |> Procedure.run CmdTagger TestTagger
          )
        |> expectMappedValue "Awesome, fun, stuff!!!"
  ]

breakTests : Test
breakTests =
  describe "when a seqeunce of commands is interrupted"
  [ test "it returns the error only" <|
    \() ->
      Elmer.given testModel emptyView testUpdate
        |> Spy.use [ stringCommandSpy, intCommandSpy ]
        |> Command.send (\_ ->
            Procedure.sequence
              [ Procedure.do <| fakeStringCommand "Awesome"
              , Procedure.break "Break!?"
              , Procedure.do <| fakeStringCommand "stuff!!!"
              , Procedure.do <| fakeStringCommand "more stuff!!!"
              ]
              |> Procedure.map (\results -> String.join ", " results)
              |> Procedure.try CmdTagger TestResultTagger
          )
        |> expectError "Break!?"
  ]

expectMappedValue : String -> TestState Model Msg -> Expect.Expectation
expectMappedValue expected testState =
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
  Spy.observe (\_ -> fakeStringCommand)
    |> andCallFake (\word tagger -> 
      tagger word |> Command.fake
    )


intCommandSpy : Spy
intCommandSpy =
  Spy.observe (\_ -> fakeIntCommand)
    |> andCallFake (\number tagger -> 
      tagger number |> Command.fake
    )


fakeStringCommand : String -> (String -> Msg) -> Cmd Msg
fakeStringCommand _ _ =
  Cmd.none


fakeIntCommand : Int -> (Int -> Msg) -> Cmd Msg
fakeIntCommand _ _ =
  Cmd.none


type Msg
  = CmdTagger (Cmd Msg)
  | TestTagger String
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
    TestTagger value ->
      ({ model | message = value }, Cmd.none)
    TestResultTagger result ->
      case result of
        Ok data ->
          ({ model | message = data }, Cmd.none)
        Err error ->
          ({ model | error = error }, Cmd.none)


emptyView : Model -> Html Msg
emptyView _ =
  Html.text ""