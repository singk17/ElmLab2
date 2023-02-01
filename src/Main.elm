module Main exposing (main)

import Browser
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import YOURNAME

type Msg = Tick Float GetKeyState

view model = collage 192 128 (YOURNAME.shapes model)
main = gameApp Tick { model = 0, view = view, update = (\x -> \y -> y), title = "PUT SOME TITLE HERE" }
