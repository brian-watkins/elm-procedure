port module SendToChannelTests exposing (..)

import Expect
import Test exposing (..)
import Elmer
import Elmer.Command as Command
import Elmer.Subscription as Subscription
import Elmer.Spy as Spy
import Elmer.Spy.Matchers exposing (calls, hasArgs, stringArg)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure
import Procedure.Channel as Channel


sendAndReceiveChannelTests : Test
sendAndReceiveChannelTests =
  describe "when open is used with connect to initialize a channel" <|
  let
      procedureState =
        Helpers.procedureCommandTestState
          |> Command.send (\_ -> 
              Channel.open (\_ -> Helpers.stringPortCommand "Fun!")
                |> Channel.connect (Helpers.stringSubscription "Triggered by port")
                |> Channel.acceptOne
                |> Procedure.map (\result -> "Mapped: " ++ result)
                |> Procedure.andThen (\result -> Procedure.fetch <| Helpers.stringCommand <| result ++ "!!!")
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
  , test "it provides the procedureId" <|
    \() ->
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Channel.open (\procedureId -> Helpers.stringPortCommand <| String.fromInt procedureId)
              |> Channel.connect (Helpers.stringSubscription "Triggered by port")
              |> Channel.acceptOne
              |> Procedure.run ProcedureTagger TestStringTagger
          )
        |> Command.send (\_ ->
            Channel.open (\procedureId -> Helpers.stringPortCommand <| String.fromInt procedureId)
              |> Channel.connect (Helpers.stringSubscription "Triggered by port")
              |> Channel.acceptOne
              |> Procedure.run ProcedureTagger TestStringTagger
          )
        |> Subscription.with (\_ -> Helpers.testSubscriptions)
        |> Subscription.send "string-subscription" "received awesome value from subscription"
        |> Spy.expect (\_ -> Helpers.stringPortCommand) (
          Elmer.expectAll
          [ calls <| Elmer.atIndex 0 <| hasArgs [ stringArg "0" ]
          , calls <| Elmer.atIndex 1 <| hasArgs [ stringArg "1" ]
          ]
        )
  ]