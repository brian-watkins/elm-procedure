port module Runner exposing
  ( browserProgram
  , skip
  , pick
  )

import Spec exposing (Message)


port elmSpecOut : Message -> Cmd msg
port elmSpecIn : (Message -> msg) -> Sub msg
port elmSpecPick : () -> Cmd msg


config : Spec.Config msg
config =
  { send = elmSpecOut
  , listen = elmSpecIn
  }


pick =
  Spec.pick elmSpecPick


skip =
  Spec.skip


browserProgram =
  Spec.browserProgram config