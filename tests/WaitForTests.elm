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
            Procedure.do (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> Procedure.waitFor <| Helpers.stringSubscription result)
              |> Procedure.andThen (\result -> Procedure.do <| Helpers.stringCommand <| "After sub: " ++ result)
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
              Procedure.do (Helpers.stringCommand "First")
                |> Procedure.andThen (\result -> Procedure.waitFor <| Helpers.stringSubscription result)
                |> Procedure.andThen (\result -> Procedure.do <| Helpers.stringCommand <| "After sub: " ++ result)
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
