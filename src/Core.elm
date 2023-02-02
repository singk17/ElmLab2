module Core exposing (TimeData,emptyTime,SharedMsg(..),CoreMsg(..))

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

type alias TimeData = {
      truetime : Float
    , time : Float
    , lasttime : Float
    , deltatime : Float
  }

emptyTime : TimeData
emptyTime = {
      truetime = 0
    , time = 0
    , lasttime = 0
    , deltatime = 0
  }

type SharedMsg =  Next Int
                | Restart

type CoreMsg a b c d e = Tick Float GetKeyState
                    | Shared SharedMsg
                    | AMsg a
                    | DMsg b
                    | KMsg c
                    | LMsg d
                    | NMsg e

