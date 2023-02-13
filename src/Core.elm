module Core exposing (TimeData,emptyTime,SharedMsg(..),CoreMsg(..),Minigame(..),restartTime,tickTime)

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

type Minigame =   Passcode
                | Leafs
                | Wires
                | Swipe

type SharedMsg =  Next Int
                | Restart
                | GotoMinigame Minigame
                | SetupMinigame Minigame

type CoreMsg a b c d e = Tick Float GetKeyState
                    | Shared SharedMsg
                    | AMsg a
                    | DMsg b
                    | KMsg c
                    | LMsg d
                    | NMsg e

restartTime : TimeData -> TimeData
restartTime timedata = {
      timedata |
      time = 0
    , lasttime = timedata.truetime
    , deltatime = 0
  }

tickTime : Float -> TimeData -> TimeData
tickTime tick timedata = {
      timedata |
      truetime = tick
    , time = tick - timedata.lasttime
    , deltatime = tick - timedata.truetime
  }
