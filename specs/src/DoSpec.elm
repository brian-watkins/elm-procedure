port module DoSpec exposing (main)

import Spec exposing (..)
import Spec.Port as Port
import Spec.Claim exposing (..)
import Spec.Extra exposing (..)
import Runner
import Json.Decode as Json
import Procedure
import Helpers exposing (..)


doSpecs =
  describe "#do"
  [ scenario "do is used to send a command without a callback" (
      runProcedure (
        Procedure.do (sendString "hello!!")
      )
      |> observeThat
        [ it "performs the command" (
            Port.observe "sendString" Json.string
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
        Procedure.endWith (sendString "hello!!")
      )
      |> observeThat
        [ it "performs the command" (
            Port.observe "sendString" Json.string
              |> expect (isListWhere
                [ equals "hello!!"
                ]
              )
          )
        , expectValues []
        ]
    )
  ]


port sendString : String -> Cmd msg


main =
  Runner.browserProgram
    [ doSpecs
    , endWithSpec
    ]