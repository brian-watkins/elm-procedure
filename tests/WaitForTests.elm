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
  describe "waitFor"
  [ test "it waits for the subscription and then continues the procedure" <|
    \() ->
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
        |> Helpers.expectValue "Mapped: After sub: First then value from subscription"
  , describe "when the subscription is received"
    [ test "it removes the subscription" <|
      \() ->
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
                    |> Procedure.waitForValue (\desc -> desc.key == result)
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
                    |> Procedure.waitForValue (\desc -> desc.key == result)
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

