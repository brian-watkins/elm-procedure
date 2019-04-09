module SubscribeToChannelTests exposing (..)

import Expect
import Test exposing (..)
import Elmer
import Elmer.Command as Command
import Elmer.Subscription as Subscription
import TestHelpers as Helpers exposing (Msg(..))
import Procedure
import Procedure.Channel as Channel


subscribeTests : Test
subscribeTests =
  describe "wait on a subscription to a channel" <|
  let
    procedureState =
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Procedure.fetch (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> 
                Channel.join (Helpers.stringSubscription result)
                  |> Channel.acceptOne
              )
              |> Procedure.andThen (\result -> Procedure.fetch <| Helpers.stringCommand <| "After sub: " ++ result)
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


subscribeAndFilterTests : Test
subscribeAndFilterTests =
  describe "when a subscription expects a particular value"
  [ describe "when the value is received"
    [ test "it processes the remainder of the procedure" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
              Procedure.provide "sub-key"
                |> Procedure.andThen (\result ->
                  Channel.join Helpers.keySubscription
                    |> Channel.filter (\_ desc -> desc.key == result)
                    |> Channel.acceptOne
                )
                |> Procedure.map .value
                |> Procedure.andThen (\result -> Procedure.fetch <| Helpers.stringCommand <| "After sub: " ++ result)
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
              Procedure.provide "sub-key"
                |> Procedure.andThen (\result ->
                  Channel.join Helpers.keySubscription
                    |> Channel.filter (\_ desc -> desc.key == result)
                    |> Channel.acceptOne
                )
                |> Procedure.map .value
                |> Procedure.andThen (\result -> Procedure.fetch <| Helpers.stringCommand <| "After sub: " ++ result)
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


filterOnProcedureIdTests : Test
filterOnProcedureIdTests =
  describe "when filtering for a specific value" <|
  let
    testState =
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Procedure.provide "something"
              |> Procedure.andThen (\result ->
                Channel.join Helpers.keySubscription
                  |> Channel.filter (\procedureId desc -> desc.key == String.fromInt procedureId)
                  |> Channel.acceptOne
              )
              |> Procedure.map .value
              |> Procedure.try ProcedureTagger TestResultTagger
          )
        |> Command.send (\_ ->
            Procedure.provide "something else"
              |> Procedure.andThen (\result ->
                  Channel.join Helpers.intSubscription
                    |> Channel.filter (\procedureId number -> number == procedureId)
                    |> Channel.acceptOne
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


multipleChannelTests : Test
multipleChannelTests =
  describe "when procedures with different channels are running" <|
  let
    testState =
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Procedure.fetch (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> 
                Channel.join (Helpers.stringSubscription result)
                  |> Channel.acceptOne
              )
              |> Procedure.andThen (\result -> Procedure.fetch <| Helpers.stringCommand <| "After sub: " ++ result)
              |> Procedure.map (\result -> "Mapped: " ++ result)
              |> Procedure.try ProcedureTagger TestResultTagger
          )
        |> Command.send (\_ ->
            Procedure.fetch (Helpers.stringCommand "Second")
              |> Procedure.andThen (\result -> 
                Channel.join Helpers.intSubscription
                  |> Channel.acceptOne
              )
              |> Procedure.andThen (\result ->
                Procedure.fetch <| Helpers.stringCommand <| "After sub: " ++ String.fromInt result
              )
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


mapTwoChannelsTests : Test
mapTwoChannelsTests =
  describe "when the procedure is waiting on two channels"
  [ test "it processes the channels sequentially" <|
    \() ->
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
          Procedure.map2 (\a b -> a ++ " AND " ++ b)
            ( Procedure.fetch (Helpers.stringCommand "First String Command")
                |> Procedure.andThen (\result ->
                  Channel.join (Helpers.stringSubscription result)
                    |> Channel.acceptOne
                )
                |> Procedure.andThen (\result -> Procedure.fetch <| Helpers.stringCommand <| "After sub: " ++ result)
            )
            ( Procedure.provide 787
                |> Procedure.andThen (\_ -> 
                  Channel.join Helpers.intSubscription
                    |> Channel.acceptOne
                )
                |> Procedure.map (\result -> "Mapped Int: " ++ String.fromInt result)
            )
            |> Procedure.try ProcedureTagger TestResultTagger
        )
        |> Subscription.with (\_ -> Helpers.testSubscriptions)
        |> Subscription.send "string-subscription" "value from subscription"
        |> Subscription.with (\_ -> Helpers.testSubscriptions)
        |> Subscription.send "int-subscription" 38
        |> Helpers.expectValue "After sub: First String Command then value from subscription AND Mapped Int: 38"
  ]
