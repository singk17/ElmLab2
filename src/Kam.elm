module Kam exposing (shapes,update,Msg(..),Model,init)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core
import Bitwise

type Msg = ButtonPress Int
          | Restart

type GameState = Game
                  | Animating

type Animation =    ShowPassword Int Int
                  | Waiting Int Animation
                  | FailLights Animation
                  | FinishGame

type alias Model = {
      correctPassword : List (List Int)
    , userPassword : List Int
    , lastBtnPress : Int
    , showerLit : Int
    , state : GameState
    , anim : Animation
    , currentPassword : Int
    , currentLights : List Int
    , btnPressTime : Float
    , finished : Bool
  }

type alias SquarePhase = {
      litSquare : Int
    , normal : Color
    , lit : Color
  }

type alias LightPhase = {
      lits : List Int
    , normal : Color
    , lit : Color
    , angry : Color
  }

init : Model
init = {
      correctPassword =  [[8],[8,6],[8,6,0],[8,6,0,6],[8,6,0,6,4]]
    , userPassword = []
    , lastBtnPress = -1
    , showerLit = -1
    , state = Animating
    , anim = Waiting 1 (ShowPassword 0 0)
    , currentPassword = -1
    , currentLights = List.map (\x -> 0) (List.range 1 5)
    , btnPressTime = -1
    , finished = False
  }


update : (Core.CoreMsg a b Msg d e) -> (Core.TimeData,Model) -> (Core.TimeData,Model)
update msg (timedata,model) = 
  case model.state of
    Game ->
      case msg of
        Core.KMsg a ->
          case a of
            Restart -> (Core.restartTime timedata,init)
            ButtonPress idx ->
              let
                newPasswordList = model.userPassword ++ [idx]
                currentPassword = getElement model.correctPassword model.currentPassword
              in
                if (List.length newPasswordList == List.length currentPassword) then
                  if (newPasswordList == currentPassword) then
                    if (model.currentPassword + 1 == List.length model.correctPassword) then
                      (
                        FinishGame
                      ) |> setupAnimation (timedata,{ model | userPassword = [1, 1, 1, 1, 1] })
                    else
                      let
                        newModel = {
                              model |
                              lastBtnPress = idx
                            , btnPressTime = timedata.truetime
                            , userPassword = newPasswordList
                          }
                      in
                      (
                        Waiting 1
                          <| ShowPassword (model.currentPassword + 1) 0
                      ) |> setupAnimation (timedata, newModel)
                  else
                    -- todo start fail lights
                    (
                      FailLights
                        <| Waiting 1
                        <| ShowPassword (model.currentPassword) 0
                    ) |> setupAnimation (timedata,model)
                else
                  (timedata, { model | btnPressTime = timedata.truetime, userPassword = newPasswordList, lastBtnPress = idx })
        Core.Tick t _ -> checkBtnTimeOut t (timedata,model)
        _ -> (timedata, model)
    Animating ->
      case (msg) of
          Core.KMsg a ->
            case a of
              Restart -> (Core.restartTime timedata,init)
              _ -> modelAnimationUpdate (timedata,model)
          Core.Tick t _ ->
              checkBtnTimeOut t (timedata,model) |> modelAnimationUpdate
          _ -> (timedata,model)


checkBtnTimeOut : Float -> (Core.TimeData,Model) -> (Core.TimeData,Model)
checkBtnTimeOut tick (timedata,model) =
  if (model.lastBtnPress /= -1) then
    if (tick - 1 > model.btnPressTime) then
      (timedata, { model | lastBtnPress = -1 })
    else
      (timedata,model)
  else
    (timedata,model)

setupAnimation : (Core.TimeData,Model) -> Animation -> (Core.TimeData,Model)
setupAnimation (timedata,model) animToSetup =
  let
      rTime = Core.restartTime timedata
  in
  
  case animToSetup of
    ShowPassword pi di ->
      let
        currentPass = getElement model.correctPassword pi
        currentDigit = getElement currentPass di
      in
      (rTime, { model | userPassword = [], currentPassword = pi, lastBtnPress = -1, state = Animating, showerLit = currentDigit, anim = animToSetup })
    Waiting x aN -> (rTime, { model | state = Animating, anim = animToSetup })
    FinishGame -> (rTime,{ model | anim = FinishGame, state = Animating, finished = True })
    FailLights nextAnim -> (rTime, { model | anim = animToSetup, state = Animating  })


modelAnimationUpdate : (Core.TimeData,Model) -> (Core.TimeData,Model)
modelAnimationUpdate (timedata,model) = 
    case model.anim of
      Waiting a nextAnim -> 
        if (timedata.time > 1) then
          if (a <= 1) then
            setupAnimation (Core.restartTime timedata, model) nextAnim
          else
            (timedata, { model | anim = Waiting (a-1) nextAnim })
        else
          (timedata,model)
      ShowPassword pi di -> 
        let
          ss = timedata
        in
        if (timedata.time > 1) then
          let
            rTime = Core.restartTime timedata
            passwordToShow = getElement model.correctPassword pi
          in
          if (di + 1 >= List.length passwordToShow) then
            (rTime,{ model | state = Game, showerLit = -1 })
          else
            (rTime,{ model | anim = ShowPassword pi (di + 1), showerLit = getElement passwordToShow (di + 1) })
        else
          (timedata,model)
      FinishGame -> (timedata,model)
      FailLights nextAnim ->
        if (timedata.time > 2) then
          setupAnimation (timedata,model) nextAnim
        else
          (timedata,model)

{-
  case msg of
    Core.KMsg a -> case a of
      ButtonPress idx -> (timedata,{ model | userPassword = model.userPassword ++ [idx], lastBtnPress = idx })
      Restart -> (Core.restartTime timedata,init)
    _ -> (timedata,model)
-}

getElement : List a -> Int -> a
getElement li idx = case (List.drop idx li |> List.head) of
      Just a -> a
      Nothing -> Debug.todo "cant handle this"

hex color =
  let
    r = color |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xff |> toFloat
    g = color |> Bitwise.shiftRightBy  8 |> Bitwise.and 0xff |> toFloat
    b = color |> Bitwise.and 0xff |> toFloat
  in
    rgb r g b

buttonShadeCutter = group
  [
      line (-7.5,-7.5) (7.5,7.5)
        |> outlined (solid 0.5) red
    , line (-7.5,7.5) (7.5,-7.5)
        |> outlined (solid 0.5) red
    , roundedRect 12 12 2
        |> filled red
  ]
buttonShade = square 15
                |> filled black
                |> subtract buttonShadeCutter
                |> makeTransparent 0.5

buttonShadow = roundedRect 16 16 2
                |> filled black
                |> move(0.7,-0.7)
                |> makeTransparent 0.6

button :  SquarePhase -> Int -> Shape (Core.CoreMsg a b Msg d e)
button phases idx =
  let
    x = modBy 3 idx |> toFloat
    y = idx // 3 |> toFloat
    fpos = ( x * 18 - 18, y * 18 - 21)
    color = if (phases.litSquare == idx) then
              phases.lit
            else
              phases.normal
  in
  group
    [
        buttonShadow
      , roundedRect 15 15 2
          |> filled color
          |> addOutline (solid 0.5) black
      , buttonShade
      {- }, text (String.fromInt idx)
          |> size 8
          |> filled black
          |> move (-4,0) -}
    ] |> move fpos
      |> notifyTap (Core.KMsg (ButtonPress idx))

createButtons : SquarePhase -> Shape (Core.CoreMsg a b Msg d e)
createButtons model = List.map (button model) (List.range 0 8) |> group

restartButton = 
  group
    [
        circle 7.8
          |> ghost
          |> makeTransparent 0
      , group
          [
              triangle 3.8
                |> filled black
                |> rotate (degrees (39))
                |> move (4,-3)
            , circle 7
                |> filled grey
                |> subtract (wedge 8 0.15 |> filled white)
                |> subtract (circle 3 |> ghost)
                |> addOutline (solid 1.2) black
            , triangle 3
                |> filled grey
                |> rotate (degrees (39))
                |> move (4,-3)
          ] |> rotate (degrees 200)
    ]



lightHighlight = oval 2 1
                  |> filled white
                  |> rotate (degrees 40)
lightShade = wedge 3 0.5
              |> filled black
              |> rotate (degrees -90)
              |> subtract (oval 6 2.5 |> ghost)
              |> rotate (degrees 30)
light : LightPhase -> Int -> Shape (Core.CoreMsg a b Msg d e)
light phases idx =
  let
    color = case (getElement phases.lits idx) of
              1 -> phases.lit
              2 -> phases.angry
              _ -> phases.normal
    x = (toFloat idx) * 12.75 - 25.5
  in
  group
    [
        circle 3.5
          |> filled black
          |> makeTransparent 0.6
          |> move (0.5,-0.7)
      , circle 3
          |> filled color
          |> addOutline (solid 0.5) black
      , lightHighlight
          |> move (-1,1)
      , lightShade
          |> makeTransparent 0.45
    ] |> move (x,0)

createLights : LightPhase -> Shape (Core.CoreMsg a b Msg d e)
createLights phases = List.map (light phases) (List.range 0 4) |> group

darkGreyColor = hex 0x414141


highlight = hex 0xcdcdcd
padBG = hex 0xa4a4a4

padCaseBackSQ = rectangle 75 85
                  |> filled padBG

padCase =
  let
    w = 75
    h = 85
    hw = w / 2
    hh = h / 2
    highDiag = 5
    o = 3
  in
  group
    [
        padCaseBackSQ
      , rectangle w h
          |> filled black
          |> makeTransparent 0.6
      , roundedRect 65 76 4
          |> filled padBG
      , line (-hw,-hh) (-hw+10,-hh+10)
          |> outlined (solid 4) padBG
          |> clip padCaseBackSQ
      , line (hw,-hh) (hw-10,-hh+10)
          |> outlined (solid 4) padBG
          |> clip padCaseBackSQ
      , polygon
          [
              (-hw,hh)
            , (-hw+highDiag,hh-highDiag-o)
            , (hw-highDiag,hh-highDiag-o)
            , (hw,hh)
          ] |> filled highlight
      , rectangle 75 85
          |> outlined (solid 1) black
    ]

btnDownColor = hex 0x3C9BE8
btnNormalColor = hex 0xd0d0d0
btnDisabledColor = hex 0x727272

phaseSquare :  SquarePhase -> Int -> Shape (Core.CoreMsg a b Msg d e)
phaseSquare phases idx =
  let
    x = modBy 3 idx |> toFloat
    y = idx // 3 |> toFloat
    fpos = ( x * 18 - 18, y * 18 - 21)
    color = if (phases.litSquare == idx) then
              phases.lit
            else
              phases.normal
  in
  group
    [
        square 17
          |> filled color
      {- }, text (String.fromInt idx)
          |> size 8
          |> filled red
          |> move (-3,0) -}
    ] |> move fpos
  
createPhaseSquares : SquarePhase -> Shape (Core.CoreMsg a b Msg d e)
createPhaseSquares phases = List.map (phaseSquare phases) (List.range 0 8) |> group

shapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg a b Msg d e))
shapes (timedata,model) =
  let
    userSquaresPhase = {
          litSquare = if (model.finished) then -1 else model.lastBtnPress
        , lit = btnDownColor
        , normal = if (model.state == Animating) then btnDisabledColor else btnNormalColor
      }
    referenceSquarePhases = {
          litSquare = if (model.finished) then -1 else model.showerLit
        , lit = btnDownColor
        , normal = black
      }

    padLightsToShow = model.currentPassword + 1
    padLightPhases = {
          lits = (List.map (\x -> 1) (List.range 1 padLightsToShow)) ++ [0, 0, 0, 0, 0]
        , lit = green
        , normal = grey
        , angry = red
      }
    userLightPhases = {
          lits =  if (model.finished) then
                    [1, 1, 1, 1, 1]
                  else
                    case (model.state) of
                      Game ->
                        (List.map (\x -> 1) (List.range 1 (List.length model.userPassword))) ++ [0, 0, 0, 0, 0]
                      Animating ->
                        case model.anim of
                          FailLights nA ->
                            let
                              cCol =  if ( (timedata.time * 10) |> floor |> modBy 3 |> (==) 0 ) then
                                        2
                                      else
                                        0
                            in
                            List.map (\x -> cCol) (List.range 1 5)
                          _ -> (List.map (\x -> 1) (List.range 1 (List.length model.userPassword))) ++ [0, 0, 0, 0, 0]
        , lit = green
        , normal = grey
        , angry = red
      }
    waitText = case (model.anim) of
      Waiting a aN -> "Waiting for " ++ (String.fromInt a) ++ "s"
      _ -> "Not Waiting"
    
    padY =  if (model.finished && timedata.time > 1) then
              -(timedata.time - 1) * 100
            else
              0
  in
    [
        bg

      , restartButton
          |> move (75,54)
          |> notifyTap (Core.KMsg Restart)
      
      , group
          [
              padCase
            , createButtons userSquaresPhase
                |> move (0,-5)
            , createLights userLightPhases
                |> move (0,28)
          ] |> move (48,padY)

      , group
          [
              padCase
            , roundedRect 55 55 2
                |> filled (hex 0xc1c1c1)
                |> move (0,-8)
            , square 52.5
                |> filled black
                |> move (0, -8)
            , createPhaseSquares referenceSquarePhases
                |> scale (0.94)
                |> move (0,-5)
            , createLights padLightPhases
                |> move (0,28)
          ] |> move (-48,padY)
      {- }, text waitText
          |> size 12
          |> filled black
          |> move (20,-60)
      , text (String.fromFloat timedata.time)
          |> size 8
          |> filled black
          |> move (-90,-60) -}
    ] ++ (
          if (model.finished) then
            let
              yPos =  if (timedata.time < 1.5) then
                        (timedata.time * 64 / 1.5) - 64
                      else
                        0
            in
            [
                text "Task Completed"
                  |> size 18
                  |> sansserif
                  |> centered
                  |> filled white
                  |> addOutline (solid 0.5) black
              , group
                  [
                      roundedRect 50 20 5
                        |> filled green
                        |> addOutline (solid 2) blue
                    , text "Next"
                        |> centered
                        |> filled blue
                        |> move (0,-4)
                  ] |> move (0,-15)
                    |> notifyTap (Core.Shared (Core.Next 1))
            ] |> List.map (move (0,yPos))
          else
            []
          )


bg = rectangle 10 20
      |> filled (hsl (degrees 354) 0.007 0.467)  
      |> rotate (degrees 90)
      |> scale 15