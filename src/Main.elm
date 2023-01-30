module Main exposing (main)

import Browser
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core

import YOURNAME

view model = model collage 192 128 YOURNAME.myShapes
main = gameApp Core.Tick { model = Core.model, view = view, update = YOURNAME.update, title = "PUT SOME TITLE HERE" }
