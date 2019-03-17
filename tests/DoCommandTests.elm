port module DoCommandTests exposing (..)

import Expect
import Test exposing (..)
import Elmer.Command as Command
import Elmer.Subscription as Subscription
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


doCommandTests : Test
doCommandTests =
  describe "when a command without a callback is triggered" <|
  let
      procedureState =
        Helpers.procedureCommandTestState
          |> Command.send (\_ -> 
              Procedure.do (Helpers.stringPortCommand "Fun!")
                |> Procedure.andThen (\_ -> Procedure.waitFor <| Helpers.stringSubscription "Triggered by port")
                |> Procedure.map (\result -> "Mapped: " ++ result)
                |> Procedure.andThen (\result -> Procedure.get <| Helpers.stringCommand <| result ++ "!!!")
                |> Procedure.run ProcedureTagger TestStringTagger
            )
  in
  [ test "it executes the command" <|
    \() -> 
      procedureState
        |> Command.expectDummy "string-port"
  , test "it continues the procedure when a subscription is received" <|
    \() ->
      procedureState
        |> Subscription.with (\_ -> Helpers.testSubscriptions)
        |> Subscription.send "string-subscription" "received awesome value from subscription"
        |> Helpers.expectValue "Mapped: Triggered by port then received awesome value from subscription!!!"
  ]
