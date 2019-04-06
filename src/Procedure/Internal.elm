module Procedure.Internal exposing
  ( ProcedureId
  , Msg(..)
  , Step(..)
  , Channel(..)
  )

type alias ProcedureId =
  Int

type Msg msg
  = Initiate (ProcedureId -> Cmd msg)
  | Execute ProcedureId (Cmd msg)
  | Subscribe ProcedureId msg (Sub msg)
  | AndThen (Cmd msg)
  | Continue


type Step e a msg
  = Step
    (ProcedureId -> (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg)

type Channel a msg
  = Channel 
    { requestGenerator : ProcedureId -> Cmd msg
    , receiver : (a -> msg) -> Sub msg
    , predicate : ProcedureId -> a -> Bool
    }
