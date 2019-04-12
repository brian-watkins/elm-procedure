module Procedure.Internal exposing
  ( ProcedureId
  , ChannelId
  , Msg(..)
  , Procedure(..)
  )

type alias ProcedureId =
  Int

type alias ChannelId =
  Int

type Msg msg
  = Initiate (ProcedureId -> Cmd msg)
  | Execute ProcedureId (Cmd msg)
  | Subscribe ProcedureId (ChannelId -> msg) (ChannelId -> Sub msg)
  | Unsubscribe ProcedureId ChannelId msg
  | Continue


type Procedure e a msg
  = Procedure
    (ProcedureId -> (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg)
