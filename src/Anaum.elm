module Anaum exposing (shapes,update,Msg(..),Model,init)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core

type Msg = None


type alias Model = {
  }

init : Model
init = {}


update : (Core.CoreMsg Msg b c d e) -> (Core.TimeData,Model) -> (Core.TimeData,Model)
update msg (timedata,model) = (timedata,model)


shapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg Msg b c d e))
shapes model = [
    
  ]
