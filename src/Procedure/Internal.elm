module Procedure.Internal exposing
  ( ProcedureId
  , ChannelId
  , Msg(..)
  , Step(..)
  , Channel
  )

type alias ProcedureId =
  Int

type alias ChannelId =
  Int

type Msg msg
  = Initiate (ProcedureId -> Cmd msg)
  | Execute ProcedureId (Cmd msg)
  | Subscribe ProcedureId msg (ChannelId -> Sub msg)
  | Unsubscribe ProcedureId ChannelId msg
  | Continue


type Step e a msg
  = Step
    (ProcedureId -> (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg)

type alias Channel a msg =
  { initialRequest : ProcedureId -> Cmd msg
  , messageHandler : (a -> msg) -> Sub msg
  , shouldProcessMessage : ProcedureId -> a -> Bool
  }
