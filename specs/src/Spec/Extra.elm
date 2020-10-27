module Spec.Extra exposing
  ( equals
  )

import Spec.Claim


equals =
  Spec.Claim.isEqual Debug.toString