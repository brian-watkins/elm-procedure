port module Helpers exposing
  ( runProcedure
  , justRunProcedure
  , runEmptyProcedure
  , tryProcedure
  , expectValue
  , expectValues
  , expectResult
  , sendValueCommand
  , intSubscription
  , stringSubscription
  , objectSubscription
  , stringPortCommand
  , testSubscriptions
  )

import Spec exposing (..)
import Spec.Setup as Setup
import Spec.Command as Command
import Spec.Observer as Observer
import Spec.Claim exposing (..)
import Spec.Extra exposing (..)
import Task
import Procedure.Program
import Procedure


runProcedure =
  run GotValue


runEmptyProcedure =
  run never


run tagger procedure =
  procedureSetup
    |> when "the procedure runs"
      [ procedure
          |> Procedure.run ProcedureMsg tagger
          |> Command.send
      ]


justRunProcedure procedure =
  procedure
    |> Procedure.run ProcedureMsg GotValue
    |> Command.send


tryProcedure procedure =
  procedureSetup
    |> when "the procedure runs"
      [ procedure
          |> Procedure.try ProcedureMsg GotResult
          |> Command.send
      ]


procedureSetup =
  given (
    Setup.initWithModel testModel
      |> Setup.withUpdate testUpdate
      |> Setup.withSubscriptions testSubscriptions
  )
  

expectValue expected =
  it "returns the expected value" (
    Observer.observeModel .values
      |> Observer.focus (isListWhereItemAt 0)
      |> expect (equals expected)
  )


expectValues expected =
  it "generates the expected values" (
    Observer.observeModel .values
      |> expect (isListWhere <| List.map equals expected)
  )


expectResult expected =
  it "returns the expected result" (
    Observer.observeModel .result
      |> Observer.focus isSomethingWhere
      |> expect (equals expected)
  )


port intSubscription : (Int -> msg) -> Sub msg
port stringSubscription : (String -> msg) -> Sub msg
port stringPortCommand : String -> Cmd msg
port objectSubscription : ({ key: String, value: String } -> msg) -> Sub msg


sendValueCommand string tagger =
  Task.succeed string
    |> Task.perform tagger


type alias Model e a =
  { procedureModel: Procedure.Program.Model (Msg e a)
  , values: List a
  , result: Maybe (Result e a)
  }


testModel =
  { procedureModel = Procedure.Program.init
  , values = []
  , result = Nothing
  }


type Msg e a
  = ProcedureMsg (Procedure.Program.Msg (Msg e a))
  | GotValue a
  | GotResult (Result e a)


testUpdate : Msg e a -> Model e a -> ( Model e a, Cmd (Msg e a) )
testUpdate msg model =
  case msg of
    ProcedureMsg procMsg ->
      Procedure.Program.update procMsg model.procedureModel
        |> Tuple.mapFirst (\updated -> { model | procedureModel = updated })
    GotValue value ->
      ( { model | values = value :: model.values }
      , Cmd.none
      )
    GotResult result ->
      ( { model | result = Just result }
      , Cmd.none
      )


testSubscriptions : Model e a -> Sub (Msg e a)
testSubscriptions model =
  Procedure.Program.subscriptions model.procedureModel