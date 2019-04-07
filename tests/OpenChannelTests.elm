module OpenChannelTests exposing (..)

import Expect
import Test exposing (..)
import Elmer
import Elmer.Command as Command
import Elmer.Subscription as Subscription
import TestHelpers as Helpers exposing (Msg(..))
import Procedure
import Procedure.Channel as Channel


awaitChannelTests : Test
awaitChannelTests =
  describe "when a procedure waits for a channel message"
  [ test "it processes only the first message" <|
    \() ->
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
          Channel.subscribe Helpers.intSubscription
            |> Procedure.await
            |> Procedure.map String.fromInt
            |> Procedure.run ProcedureTagger TestStringAccumulator
        )
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 14
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 8
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 27
        |> Helpers.expectValues [ "14" ]
  ]

openChannelTests : Test
openChannelTests =
  describe "when a channel is open"
  [ test "it continually processes incoming messages" <|
    \() ->
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
          Channel.subscribe Helpers.intSubscription
            |> Procedure.open
            |> Procedure.map String.fromInt
            |> Procedure.run ProcedureTagger TestStringAccumulator
        )
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 14
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 8
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 27
        |> Helpers.expectValues [ "27", "8", "14" ]
  , describe "when the channel is filtered"
    [ test "it continually processes the filtered messages" <|
      \() ->
        Helpers.procedureCommandTestState
        |> Command.send (\_ ->
          Channel.subscribe Helpers.intSubscription
            |> Channel.filter (\_ num -> modBy 2 num == 0)
            |> Procedure.open
            |> Procedure.map String.fromInt
            |> Procedure.run ProcedureTagger TestStringAccumulator
        )
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 14
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 9
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 8
        |> Helpers.expectValues [ "8", "14" ]
    ]
  ]


openChannelAndAwaitTests : Test
openChannelAndAwaitTests =
  describe "when a channel is open and the procedure waits on another channel"
  [ test "it processes all incoming messages to the open channel and only the first on the waiting channel" <|
    \() ->
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
          Channel.subscribe Helpers.intSubscription
            |> Procedure.open
            |> Procedure.map String.fromInt
            |> Procedure.andThen (\result ->
              Helpers.stringSubscription result
                |> Channel.subscribe
                |> Procedure.await
            )
            |> Procedure.map (\r -> "After awaiting: " ++ r)
            |> Procedure.run ProcedureTagger TestStringAccumulator
        )
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 14
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "string-subscription" "one"
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "string-subscription" "another one"
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 8
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "string-subscription" "two"
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "string-subscription" "another one"
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "int-subscription" 27
        |> Subscription.with (\_ -> Helpers.testSubscriptionsWithExtraSubs)
        |> Subscription.send "string-subscription" "three"
        |> Helpers.expectValues
          [ "After awaiting: 27 then three"
          , "After awaiting: 8 then two"
          , "After awaiting: 14 then one"
          ]
  ]
