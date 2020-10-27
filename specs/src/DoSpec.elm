module DoSpec exposing (main)

import Spec exposing (..)
import Spec.Port as Port
import Spec.Claim exposing (..)
import Spec.Extra exposing (..)
import Runner
import Json.Decode as Json
import Procedure
import Time
import Helpers exposing (..)


doSpecs =
  describe "#do"
  [ scenario "do is used to send a command without a callback" (
      runProcedure (
        Procedure.do (stringPortCommand "hello!!")
      )
      |> observeThat
        [ it "performs the command" (
            Port.observe "stringPortCommand" Json.string
              |> expect (isListWhere
                [ equals "hello!!"
                ]
              )
          )
        , expectValue ()
        ]
    )
  ]


endWithSpec =
  describe "#endWith"
  [ scenario "end procedure with a command that has no callback" (
      runEmptyProcedure (
        Procedure.fromTask Time.now
          |> Procedure.map Time.posixToMillis
          |> Procedure.andThen (\millis -> 
              stringPortCommand ("Time: " ++ String.fromInt millis)
                |> Procedure.endWith
          )
      )
      |> observeThat
        [ it "performs the command" (
            Port.observe "stringPortCommand" Json.string
              |> expect (isListWhere
                [ isStringContaining 1 "Time"
                ]
              )
          )
        , expectValues []
        ]
    )
  ]


main =
  Runner.browserProgram
    [ doSpecs
    , endWithSpec
    ]