module Main exposing (main)

import Browser
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core

import Anaum
import Dunya
import Kam
import Lakysha
import Neaha


type alias Msg = Core.CoreMsg Anaum.Msg Dunya.Msg Kam.Msg Lakysha.Msg Neaha.Msg

type Stage =  StartScreen
              | A | D | K | L | N

type alias Model = {
      timedata: Core.TimeData
    , amodel: Anaum.Model
    , dmodel: Dunya.Model
    , kmodel: Kam.Model
    , lmodel: Lakysha.Model
    , nmodel: Neaha.Model
    , stage: Stage
  }

init : Model
init = {
      timedata = Core.emptyTime
    , amodel = Anaum.init
    , dmodel = Dunya.init
    , kmodel = Kam.init
    , lmodel = Lakysha.init
    , nmodel = Neaha.init
    , stage = StartScreen
  }



mainUpdate : (Core.CoreMsg Anaum.Msg Dunya.Msg Kam.Msg Lakysha.Msg Neaha.Msg) -> Model -> Model
mainUpdate msg model =
  case msg of
    Core.Tick t kd ->
      let
        updatedTime = { model | timedata = basicUpdate model.timedata t }
      in
        case model.stage of
          StartScreen -> updatedTime
          A -> { updatedTime | amodel = Tuple.second <| Anaum.update msg (updatedTime.timedata, updatedTime.amodel) }
          D -> { updatedTime | dmodel = Tuple.second <| Dunya.update msg (updatedTime.timedata, updatedTime.dmodel) }
          K -> { updatedTime | kmodel = Tuple.second <| Kam.update msg (updatedTime.timedata, updatedTime.kmodel) }
          L -> { updatedTime | lmodel = Tuple.second <| Lakysha.update msg (updatedTime.timedata, updatedTime.lmodel) }
          N -> { updatedTime | nmodel = Tuple.second <| Neaha.update msg (updatedTime.timedata, updatedTime.nmodel) }
    Core.Shared (Core.Next a) -> model
    Core.Shared (Core.Restart) -> model
    Core.AMsg a -> { model | amodel = Tuple.second <| Anaum.update msg (model.timedata, model.amodel) }
    Core.DMsg a -> { model | dmodel = Tuple.second <| Dunya.update msg (model.timedata, model.dmodel) }
    Core.KMsg a -> { model | kmodel = Tuple.second <| Kam.update msg (model.timedata, model.kmodel) }
    Core.LMsg a -> { model | lmodel = Tuple.second <| Lakysha.update msg (model.timedata, model.lmodel) }
    Core.NMsg a -> { model | nmodel = Tuple.second <| Neaha.update msg (model.timedata, model.nmodel) }

switchSVG : Model -> List (Shape (Core.CoreMsg Anaum.Msg Dunya.Msg Kam.Msg Lakysha.Msg Neaha.Msg))
switchSVG model =
  case model.stage of
    StartScreen -> startScreenShapes (model.timedata,model)
    A -> Anaum.shapes (model.timedata,model.amodel)
    D -> Dunya.shapes (model.timedata,model.dmodel)
    K -> Kam.shapes (model.timedata,model.kmodel)
    L -> Lakysha.shapes (model.timedata,model.lmodel)
    N -> Neaha.shapes (model.timedata,model.nmodel)

view : Model -> Collage (Core.CoreMsg Anaum.Msg Dunya.Msg Kam.Msg Lakysha.Msg Neaha.Msg)
view model = collage 192 128 (switchSVG model)

main : GameApp Model Msg
main = gameApp Core.Tick { model = init, view = view, update = mainUpdate, title = "PUT SOME TITLE HERE" }


basicUpdate : Core.TimeData -> Float -> Core.TimeData
basicUpdate timedata tick = {
      timedata |
      truetime = tick
    , time = tick - timedata.lasttime
    , deltatime = tick - timedata.truetime
  }

startScreenShapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg Anaum.Msg Dunya.Msg Kam.Msg Lakysha.Msg Neaha.Msg))
startScreenShapes (timedata,model) =
  let
    posX = (sin <| 3 * timedata.time) * 80
  in
    [
      circle 5
        |> filled red
        |> move (posX,0)
    ]
