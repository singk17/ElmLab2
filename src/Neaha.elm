module Neaha exposing (shapes,update,Msg(..),Model,init)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core


type Msg = NewPos Int (Float,Float)
                  | MovePos (Float,Float)
                  | LetGo (Float,Float)

type alias Model = { time : Float
    , pos : (Float,Float)
    , leafbeingpressed : Int
    , state : State 
    , offset : Float
    , leafState : List Float
    , fx : Float
    , fy : Float
    , screen : Screen
    }

type Screen = Success | Fail | Game

type State =  Waiting
  | Dragging 
      (Float,Float)

init : Model
init = { time = 0, pos = (0,0), state = Waiting, leafbeingpressed = 0, offset = 0, leafState = [1,1,1,1,1], fx = 0, screen = Game, fy = 0}

sub (x,y) (u,v) = (x-u,y-v)
add (x,y) (u,v) = (x+u,y+v)

update : (Core.CoreMsg a b c d Msg) -> (Core.TimeData,Model) -> (Core.TimeData,Model)
update msg (timedata,model) = case msg of 
      Core.Tick t _ -> case model.screen of 
                        Game ->
                          if (timedata.time > 30)
                            then (Core.restartTime timedata, {init | screen = Fail})
                          else (timedata,model)
                        Fail -> if (timedata.time > 3)
                                  then (Core.restartTime timedata, init)
                                else (timedata,model)
                        _ -> (timedata,model)
      Core.NMsg a -> case a of
        NewPos whichLeaf pos -> 
          case model.state of 
            Waiting ->
              (timedata,{ model | pos = pos, state = Dragging (sub model.pos pos), leafbeingpressed = whichLeaf })
            _ ->
              (timedata, model)
        MovePos (x,y) -> 
          case model.state of 
            Dragging _ -> 
              let
                fx = if (x > 50) then 50
                      else if (x < -50) then -50
                      else x
                fy = if (y > 50) then 50
                     else if (y < -50) then -50
                     else y
              in
                (timedata, { model | pos = (fx,fy) })
            _ ->
              (timedata, model)
        LetGo (x,y) -> case model.state of 
                    Dragging delta -> if (x > -45 && x< -25 && y > -30 && y < 20)
                                        then (let 
                                               newleafState = List.map (\idx -> if (idx == model.leafbeingpressed ) then 0 else (getElement model.leafState idx )) (List.range 0 4)
                                             in 
                                                (if (newleafState == [0,0,0,0,0])
                                                   then (timedata, { model | screen = Success})
                                                else (timedata, { model | leafState = newleafState, pos = add model.pos delta, state = Waiting }))
                                        )
                                      else (timedata, { model | pos = add model.pos delta, state = Waiting })
                    _ -> 
                      (timedata, model) 
      _ -> (timedata,model)


getElement : List a -> Int -> a 
getElement list idx = case (List.head (List.drop idx list)) of 
  Just a -> a 
  Nothing -> Debug.todo "list out of bounds"


myBackground = square 200 |> filled (hsl (degrees 0) 0.006 0.4)

shapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg a b c d Msg))
shapes (timedata,model) = 
     case model.screen of
     Success -> [square 100
                |> filled green
                |> makeTransparent 0.5
                , text "SUCCESS"
                |> filled green
                |> move (-25,30)
                , group [roundedRect 40 20 5
                            |> filled green
                         ,text "Next"
                            |> centered
                            |> size 8
                            |> filled black
                ]
                |> notifyTap (Core.Shared (Core.Next 1))]
     Fail -> [square 100
                |> filled red
                |> makeTransparent 0.5
                , text "Fail"
                |> filled red
                |> move (-25,0)] 
     Game -> [myBackground
      , square 100
      |> filled (hsl (degrees 191) 0.791 0.782) 
      |> addOutline (solid 1) black
      , rectangle 30 100
      |> filled grey
      |> addOutline (solid 1) black
      |> move (-35, 0)
      , text "TIME"
      |> filled black
      |> scale 0.5
      |> move (-45,43)
      , text (String.fromInt (floor timedata.time))
      |> filled black
      |> scale 0.5
      |> move (-45,35)
      , leaf1
      |> move ( case model.state of 
                      Waiting -> if (model.pos == (-35, 4 * sin (timedata.time) - 5))
                                    then model.pos  
                                else (0, 4 * sin timedata.time + 40)
                      Dragging delta -> if (model.leafbeingpressed == 0)
                                          then model.pos 
                                        else (0, 4 * sin timedata.time + 40 )
                  )
      |> ( case model.state of 
                  Waiting -> notifyMouseDownAt (\x -> Core.NMsg (NewPos 0 x))
                  Dragging _ -> identity
              )
      |> makeTransparent (getElement model.leafState 0)
      , leaf2
      |> move ( case model.state of 
                      Waiting -> (10, 4 * sin (timedata.time - 20) - 20)
                      Dragging delta -> if (model.leafbeingpressed == 1)
                                          then model.pos 
                                        else (10, 4 * sin (timedata.time - 20) - 20)
                  )
      |> ( case model.state of 
                  Waiting -> notifyMouseDownAt (\x -> (Core.NMsg (NewPos 1 x)))
                  Dragging _ -> identity
              )
      |> makeTransparent (getElement model.leafState 1)
      , leaf3
      |> move ( case model.state of 
                      Waiting -> (30, 4 * sin (timedata.time + 10) + 20)
                      Dragging delta -> if (model.leafbeingpressed == 2)
                                          then model.pos 
                                        else (30, 4 * sin (timedata.time + 10) + 20)
                  )
      |> ( case model.state of 
                  Waiting -> notifyMouseDownAt (\x -> (Core.NMsg (NewPos 2 x)))
                  Dragging _ -> identity
              )
      |> makeTransparent (getElement model.leafState 2)
      , leaf4
      |> move ( case model.state of 
                      Waiting -> (-5, 4 * sin (timedata.time + 10))
                      Dragging delta -> if (model.leafbeingpressed == 3)
                                          then model.pos 
                                        else (-5, 4 * sin (timedata.time + 10))
                  )
      |> ( case model.state of 
                  Waiting -> notifyMouseDownAt (\x -> (Core.NMsg (NewPos 3 x)))
                  Dragging _ -> identity
              )
      |> makeTransparent (getElement model.leafState 3)
      , leaf5
      |> move ( case model.state of 
                      Waiting -> (30, 4 * sin (timedata.time + 40) - 20)
                      Dragging delta -> if (model.leafbeingpressed == 4)
                                          then model.pos 
                                        else (30, 4 * sin (timedata.time + 40) - 20)
                  )
      |> ( case model.state of 
                  Waiting -> notifyMouseDownAt (\x -> (Core.NMsg (NewPos 4 x)))
                  Dragging _ -> identity
              )
      |> makeTransparent (getElement model.leafState 4)
      , window 
      |> move (-35,-5)
      , circle 5
      |> filled red
      |> move (-35,-5)
      ]
      ++
        ( case model.state of 
            Waiting -> []
            Dragging _ ->
              [rect 190 126 |> filled (rgba 0 0 0 0.01)
                |> notifyMouseMoveAt (\x ->(Core.NMsg (MovePos x)))
                |> notifyMouseUpAt (\x -> Core.NMsg (LetGo x) )
                |> notifyLeaveAt (\x -> Core.NMsg (LetGo x))
              ]
        ) 

window = group [
  rectangle 20 50
    |> filled black
  , 
  polygon [(0,0),(0,-10),(20,0)]
    |> filled black
    |> move (-10,-25)
  ,
  polygon [(0,0),(0,10),(20,0)]
    |> filled black
    |> move (-10,25)
  ]

leaf1 = group [triangle 6
    |> filled green
    |> rotate (degrees 90)
    , triangle 6
    |> filled green
    |> rotate (degrees 100)
    |> move (-5,-5)
    , triangle 6
    |> filled green
    |> rotate (degrees 70)
    |> move (5,-5)
    , triangle 4
    |> filled green
    |> rotate (degrees -90)
    |> move (0,-4)
    , curve (0,0) [Pull (10,0) (20,-10)]
    |> filled (hsl (degrees 31) 0.758 0.304)
    |> rotate (degrees -80)
    |> move (0,-10)
    , triangle 9
    |> filled green
    |> rotate (degrees -90)
    |> move (0, -10)
    ,curve (0,0) [Pull (20,10) (30,10)]
    |> filled black
    |> move (-25,-8)
    |> rotate (degrees 70)
    |> scale 0.75]|> scale 0.5

leaf2 = group [
    curve (0,0) [Pull (10,0) (20,-10)]
    |> filled (hsl (degrees 36) 0.801 0.295)
    |> rotate (degrees -65)
    |> move (0,-10)
  ,oval 7 40
    |> filled (hsl (degrees 36) 0.5 0.5)
  , curve (0,0) [Pull (5,0) (30,-10)]
    |> filled black
    |> rotate (degrees -70)
    |> move (0, 15)]    |> scale 0.75

leaf3 = group[curve (0,0) [Pull (10,0) (20,-10)]
             |> filled black
             |> rotate (degrees -65)
             |> move (0,5)
             , oval 6 23
             |> filled (hsl (degrees 125) 0.381 0.49)
             |> move (0,8)
             |> rotate (degrees 25)
             , oval 6 23
             |> filled (hsl (degrees 125) 0.381 0.49)
             |> move (0,8)
             , oval 6 23
             |> filled (hsl (degrees 125) 0.381 0.49)
             |> move (0,8)
             |> rotate (degrees -25)
             , curve (0,0) [Pull (5,0) (20,-10)]
             |> filled black
             |> rotate (degrees -65)
             |> move (0,20)
             , curve (0,0) [Pull (5,0) (20,-10)]
             |> filled black
             |> rotate (degrees 90)
             |> move (-1,-2)
             , curve (0,0) [Pull (5,0) (20,-10)]
             |> filled black
             |> rotate (degrees 140)
             |> move (1,-3)
             ]|> scale 0.75

leaf4 = group[triangle 15
             |> filled (hsl (degrees 85) 0.5 0.5)
             |> rotate (degrees -90)
             , wedge 9 0.5
             |> filled (hsl (degrees 85) 0.5 0.5)
             |> rotate (degrees 90)
             |> move(-4,7)
             , wedge 9 0.5
             |> filled (hsl (degrees 85) 0.5 0.5)
             |> rotate (degrees 90)
             |> move(4,7)
             ,rect 1 7
             |> filled black
             |> rotate (degrees 50)
             |> move (3,0)
             ,rect 1 7
             |> filled black
             |> rotate (degrees 50)
             |> mirrorY
             |> move (-3,0)
             ,rect 1 7
             |> filled black
             |> rotate (degrees 50)
             |> move (3,6)
             ,rect 1 7
             |> filled black
             |> rotate (degrees 50)
             |> mirrorY
             |> move (-3,6)
             , roundedRect 1 35 5
             |> filled black
             |> move (0,3)
             |> rotate (degrees 0)
             ]|> scale 0.75
            
cloverleaf = group[triangle 19
             |> filled green
             |> rotate (degrees -90)
             , wedge 9 0.5
             |> filled green
             |> rotate (degrees 90)
             |> move(-7,9)
             , wedge 9 0.5
             |> filled green
             |> rotate (degrees 90)
             |> move(7,9)]

leaf5 = group[ roundedRect 5 50 5
            |> filled black
            |> rotate (degrees 40)
            |> move (15,-30)
            ,cloverleaf
            ,cloverleaf
            |> rotate (degrees 90)
            |>move (-15,-15)
            ,cloverleaf
            |> rotate (degrees -90)
            |> move (15,-15)
            , cloverleaf
            |> rotate (degrees 180)
            |> move (0,-30)
            ]|> scale 0.25

withDepth : Float -> Shape Msg -> (Float, Shape Msg)
withDepth depth shape = (depth, shape)
--npx elm-live src/Main.elm
