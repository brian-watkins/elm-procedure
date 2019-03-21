module WaitForTests exposing (..)

import Expect
import Test exposing (..)
import Elmer
import Elmer.Command as Command
import Elmer.Subscription as Subscription
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


waitForTests : Test
waitForTests =
  describe "waitFor" <|
  let
    procedureState =
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Procedure.get (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> Procedure.waitFor <| Helpers.stringSubscription result)
              |> Procedure.andThen (\result -> Procedure.get <| Helpers.stringCommand <| "After sub: " ++ result)
              |> Procedure.map (\result -> "Mapped: " ++ result)
              |> Procedure.try ProcedureTagger TestResultTagger
          )
        |> Subscription.with (\_ -> Helpers.testSubscriptions)
        |> Subscription.send "string-subscription" "value from subscription"
  in
  [ test "it waits for the subscription and then continues the procedure" <|
    \() ->
      procedureState
        |> Helpers.expectValue "Mapped: After sub: First then value from subscription"
  , describe "when the subscription is received"
    [ test "it removes the subscription" <|
      \() ->
        procedureState
          |> Elmer.expectModel (\model ->
            Helpers.testSubscriptions model
              |> Expect.equal Sub.none
          )
    ]
  ]


waitForValueTests : Test
waitForValueTests =
  describe "when a subscription expects a particular value"
  [ describe "when the value is received"
    [ test "it processes the remainder of the procedure" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
              Procedure.send "sub-key"
                |> Procedure.andThen (\result ->
                  Helpers.keySubscription
                    |> Procedure.waitForValue (\_ desc -> desc.key == result)
                )
                |> Procedure.map .value
                |> Procedure.andThen (\result -> Procedure.get <| Helpers.stringCommand <| "After sub: " ++ result)
                |> Procedure.map (\result -> "Mapped: " ++ result)
                |> Procedure.try ProcedureTagger TestResultTagger
            )
          |> Subscription.with (\_ -> Helpers.testSubscriptions)
          |> Subscription.send "key-subscription" { key = "sub-key", value = "awesome value" }
          |> Helpers.expectValue "Mapped: After sub: awesome value"
    ]
  , describe "when the wrong value is received"
    [ test "it ignores that value and continues to wait" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
              Procedure.send "sub-key"
                |> Procedure.andThen (\result ->
                  Helpers.keySubscription
                    |> Procedure.waitForValue (\_ desc -> desc.key == result)
                )
                |> Procedure.map .value
                |> Procedure.andThen (\result -> Procedure.get <| Helpers.stringCommand <| "After sub: " ++ result)
                |> Procedure.map (\result -> "Mapped: " ++ result)
                |> Procedure.try ProcedureTagger TestResultTagger
            )
          |> Subscription.with (\_ -> Helpers.testSubscriptions)
          |> Subscription.send "key-subscription" { key = "wrong-key", value = "awesome value" }
          |> Subscription.with (\_ -> Helpers.testSubscriptions)
          |> Subscription.send "key-subscription" { key = "sub-key", value = "fun value" }
          |> Helpers.expectValue "Mapped: After sub: fun value"
    ]
  ]


waitForValueProcedureIdTests : Test
waitForValueProcedureIdTests =
  describe "when waiting for a specific value" <|
  let
    testState =
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Procedure.send "something"
              |> Procedure.andThen (\result ->
                Helpers.keySubscription
                  |> Procedure.waitForValue (\procedureId desc -> desc.key == String.fromInt procedureId)
              )
              |> Procedure.map .value
              |> Procedure.try ProcedureTagger TestResultTagger
          )
        |> Command.send (\_ ->
            Procedure.send "something else"
              |> Procedure.andThen (\result ->
                Helpers.intSubscription
                  |> Procedure.waitForValue (\procedureId number -> number == procedureId)
              )
              |> Procedure.map String.fromInt
              |> Procedure.try ProcedureTagger TestResultTagger
          )
        |> Subscription.with (\_ -> Helpers.testSubscriptions)
  in
  [ test "it provides the first procedure with the procedureId" <|
    \() ->
      testState
        |> Subscription.send "key-subscription" { key = "0", value = "awesome value" }
        |> Helpers.expectValue "awesome value"
  , test "it provides the second procedure with the procedureId" <|
    \() ->
      testState
        |> Subscription.send "int-subscription" 1
        |> Helpers.expectValue "1"
  ]


waitForMultipleTests : Test
waitForMultipleTests =
  describe "when procedures with different subscriptions are running" <|
  let
    testState =
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Procedure.get (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> Procedure.waitFor <| Helpers.stringSubscription result)
              |> Procedure.andThen (\result -> Procedure.get <| Helpers.stringCommand <| "After sub: " ++ result)
              |> Procedure.map (\result -> "Mapped: " ++ result)
              |> Procedure.try ProcedureTagger TestResultTagger
          )
        |> Command.send (\_ ->
            Procedure.get (Helpers.stringCommand "Second")
              |> Procedure.andThen (\result -> Procedure.waitFor <| Helpers.intSubscription)
              |> Procedure.andThen (\result -> Procedure.get <| Helpers.stringCommand <| "After sub: " ++ String.fromInt result)
              |> Procedure.map (\result -> "Mapped: " ++ result)
              |> Procedure.try ProcedureTagger TestResultTagger
        )
  in
  [ describe "when the first subscription is received" <|
    let
      firstSubState =
        testState
          |> Subscription.with (\_ -> Helpers.testSubscriptions)
          |> Subscription.send "string-subscription" "value from subscription"
    in
    [ test "it completes the first procedure" <|
      \() ->
        firstSubState
          |> Helpers.expectValue "Mapped: After sub: First then value from subscription"
    , describe "when the second subscription is received" <|
      let
        secondSubState =
          firstSubState
            |> Subscription.with (\_ -> Helpers.testSubscriptions)
            |> Subscription.send "int-subscription" 38
      in
      [ test "it completes the second procedure" <|
        \() ->
          secondSubState
            |> Helpers.expectValue "Mapped: After sub: 38"
      ]
    ]
  ]