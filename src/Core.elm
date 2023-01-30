import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

type alias Core {
    truetime: Float
    , time: Float
    , lasttime: Float
    , deltatime: Float
}

type Msg = Tick Float GetKeyState

