module Main exposing (main)

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
    , stage = D
  }



mainUpdate : (Core.CoreMsg Anaum.Msg Dunya.Msg Kam.Msg Lakysha.Msg Neaha.Msg) -> Model -> Model
mainUpdate msg model =
  case msg of
    Core.Tick t kd ->
      let
        updatedTime = { model | timedata = Core.tickTime t model.timedata }
      in
        case model.stage of
          StartScreen -> updatedTime
          A ->
            let
              (updTime,newModel) = Anaum.update msg (updatedTime.timedata, updatedTime.amodel)
            in
            { model | timedata = updTime, amodel = newModel }
          D ->
            let
              (updTime,newModel) = Dunya.update msg (updatedTime.timedata, updatedTime.dmodel)
            in
            { model | timedata = updTime, dmodel = newModel }
          K ->
            let
              (updTime,newModel) = Kam.update msg (updatedTime.timedata, updatedTime.kmodel)
            in
            { model | timedata = updTime, kmodel = newModel }
          L ->
            let
              (updTime,newModel) = Lakysha.update msg (updatedTime.timedata, updatedTime.lmodel)
            in
            { model | timedata = updTime, lmodel = newModel }
          N ->
            let
              (updTime,newModel) = Neaha.update msg (updatedTime.timedata, updatedTime.nmodel)
            in
            { model | timedata = updTime, nmodel = newModel }
    Core.Shared a ->
      case a of
          Core.Next succ ->
            let
              (_,newModel) = Dunya.update msg (model.timedata, model.dmodel)
            in
              { init | timedata = Core.restartTime model.timedata, dmodel = newModel, stage = D }
          Core.Restart -> { init | timedata = Core.restartTime model.timedata }
          Core.GotoMinigame mini ->
            let
              (_, newModel) = Debug.log "mupd" <| Dunya.update (Core.Shared (Core.SetupMinigame mini)) (model.timedata, model.dmodel)
              nextStage = case mini of
                Core.Leafs -> N
                Core.Swipe -> A
                Core.Passcode -> K
                Core.Wires -> L
            in
              { model | timedata = Core.restartTime model.timedata, dmodel = newModel, stage = nextStage }
          _ -> model
    Core.AMsg a ->
      let
        (ntime,nmodel) = Anaum.update msg (model.timedata, model.amodel)
      in
        { model | amodel = nmodel, timedata = ntime }
    Core.DMsg a ->
      let
        (ntime,nmodel) = Dunya.update msg (model.timedata, model.dmodel)
      in
        { model | dmodel = nmodel, timedata = ntime }
    Core.KMsg a ->
      let
        (ntime,nmodel) = Kam.update msg (model.timedata, model.kmodel)
      in
        { model | kmodel = nmodel, timedata = ntime }
    Core.LMsg a ->
      let
        (ntime,nmodel) = Lakysha.update msg (model.timedata, model.lmodel)
      in
        { model | lmodel = nmodel, timedata = ntime }
    Core.NMsg a ->
      let
        (ntime,nmodel) = Neaha.update msg (model.timedata, model.nmodel)
      in
        { model | nmodel = nmodel, timedata = ntime }

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
main = gameApp Core.Tick { model = init, view = view, update = mainUpdate, title = "Among Us (Bootleg)" }

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
