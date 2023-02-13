module Anaum exposing (shapes,update,Msg(..),Model,init)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core
import Html exposing (a)

type Msg = NewPos (Float,Float)
          | MovePos (Float,Float)
          | LetGo

type State 
  = Waiting
  | Dragging 
      (Float,Float)

type alias Model = { 
                    pos : (Float, Float)
                   , state : State
                   , swipeTime: Float
                   , fail1 : String
                   , fail2 : String
                   , success : String
                   , lastmsgtime : Float
                   , succeeded : Bool
                   }

init : Model
init = { pos = (0,-31), state = Waiting, swipeTime = 0, fail1 = "", fail2 = "", success = "", lastmsgtime = 0, succeeded = False }


update : (Core.CoreMsg Msg b c d e) -> (Core.TimeData,Model) -> (Core.TimeData,Model)
update msg (timedata,model) = case msg of
  Core.AMsg a -> case a of
    NewPos pos -> 
      case model.state of 
        Waiting ->
          (timedata,{ model | pos = pos, state = Dragging (sub model.pos pos), swipeTime = 0 })
        _ ->
          (timedata,model)                         
    MovePos pos -> 
      case model.state of 
        Dragging _ ->
          (timedata,{ model | pos = pos })
        _ ->
          (timedata,model)
    LetGo -> case model.state of 
        Dragging delta -> 
            if (model.pos < (40, 18)) then
              (timedata, {model | fail1 = "Bad read, try again", pos = (-50, 18), state = Waiting, lastmsgtime = timedata.truetime  })
            else if (model.swipeTime > 1) && (model.swipeTime < 2)then 
              (timedata, {model | success = "Successful", pos = (-50, 18), state = Waiting, lastmsgtime = timedata.truetime, succeeded = True })
            else if (model.swipeTime > 2) then 
              (timedata, {model | fail2 = "Too slow, try again", pos = (-50, 18), state = Waiting, lastmsgtime = timedata.truetime })
            else if (model.swipeTime < 1) then
              (timedata, {model | fail2 = "Too fast, try again", pos = (-50, 18), state = Waiting, lastmsgtime = timedata.truetime })
            else 
              (timedata, model)
        _ -> 
          (timedata,model)
  Core.Tick t (k,_,_) ->
    let 
      newmodel = if ( (t - model.lastmsgtime) > 0.5) then 
                  {model | fail1 = "", fail2 = ""}
                 else 
                  model 
      in
    case model.state of
      Dragging a -> (timedata,{ newmodel | swipeTime = model.swipeTime + timedata.deltatime})
      _ -> (timedata,newmodel) 
  _ -> (timedata,model)


doNothing : a -> a
doNothing a = a

shapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg Msg b c d e))
shapes (timedata,model) = 
  [
    rect 200 135 |> filled (hsl (degrees 0) 0.014 0.34)
  , rect 110 105 |> filled (hsl (degrees 354) 0.014 0.46)
  , rect 110 10 |> filled (hsl (degrees 354) 0.014 0.408)
  , rect 110 50 |> filled (hsl (degrees 0) 0.014 0.28)
      |> move (0,25)
  , smallgray (hsl (degrees 0) 0.039 0.612)
  , smallgray (hsl (degrees 0) 0.051 0.768)
      |> scaleX 0.96
      |> scaleY 0.80
      |> move (0,1.5)
  , curve (0,0) [Pull (10,0) (20,-10)]
    |> filled (hsl (degrees 16) 0.951 0.265)
    |> rotate (degrees 26.8)
    |> scale 3.48
    |> move (-39,-28)
  , card
      |> move (
                if (not model.succeeded) then case model.state of
                  Waiting -> model.pos
                  Dragging delta -> let (x,y) = add delta model.pos in (x,18)
                else
                  model.pos
              )
      |> ( if (not model.succeeded) then case model.state of 
             Waiting ->
               notifyMouseDownAt (\x -> (Core.AMsg (NewPos x)))
             Dragging _ ->
               identity 
            else doNothing
          )
  , biggray (hsl (degrees 0) 0.039 0.612)
  , biggray (hsl (degrees 0) 0.051 0.768)
      |> scaleX 0.96
      |> scaleY 0.86
      |> move (0, 5.5)
  , circle 3 |> filled (hsl (degrees 0) 0.014 0.28)
      |> move (40,29.5)
  , circle 3 |> filled red
      |> move (39,30)
  , circle 3 |> filled (hsl (degrees 0) 0.014 0.28)
      |> move (48,29.5)
  , circle 3 |> filled (hsl (degrees 151) 0.816 0.34)
      |> move (47,30)
  , roundedRect 40 25 5 |> filled black
      |> move (-31, 38)
  , roundedRect 40 25 5 |> filled (hsl (degrees 0) 0.051 0.768)
      |> move (-29.7, 44)
      |> scaleX 0.97
      |> scaleY 0.90
  , ngon 6 9 
      |> filled (hsl (degrees 0) 0.051 0.768)
      |> move (-43, 49)
      |> scaleY 0.90
  , rect 10 4.2 |> filled(hsl (degrees 0) 0.051 0.768)
      |> move (-47, 47.9)
  , ngon 6 10 
      |> filled (hsl (degrees 0) 0.051 0.768)
      |> move (-10,50)
      |> scaleY 0.60
  , rect 80 25 |> filled (hsl (degrees 354) 0.014 0.408)
      |> move (2,-40)
  , openPolygon [(0,0),(3,-6),(25,0)]
    |> filled (hsl (degrees 354) 0.014 0.408)
    |> rotate (degrees 272)
    |> move (-40,-27.5)
  , rect 75 8 |> filled (hsl (degrees 0) 0.039 0.612)
      |> move (0,43.5)
  , rect 75 8 |> filled (hsl (degrees 117) 0.891 0.221)
      |> move (0,45)
  , openPolygon [(0,0),(0,-3),(30,0)]
    |> filled (hsl (degrees 16) 0.936 0.34)
    |> rotate (degrees 180)
    |> scaleX 1.2
    |> move (39,-27.8)
  , openPolygon [(0,0),(0,-3),(25,0)]
    |> filled (hsl (degrees 16) 0.936 0.34)
    |> rotate (degrees 270)
    |> move (-38.95,-27.5)
  , curve (0,0) [Pull (10,0) (20,-10)]
    |> filled (hsl (degrees 16) 0.936 0.34)
    |> rotate (degrees 26)
    |> scale 1.5
    |> scaleX 1.1
    |> move (-42, -27.5)
  , rect 78 25 |> filled (hsl (degrees 16) 0.936 0.34)
      |> move (0,-40)
  , rect 7 25 |> filled (hsl (degrees 16) 0.921 0.243)
    |> move (0,-40)
  , rect 2 25 |> filled (hsl(degrees 16) 0.913 0.220)
    |> move (-3,-40)
  , rect 2 25 |> filled (hsl(degrees 16) 0.913 0.220)
    |> move (4,-40)
  , rect 19 21 |> filled black
    |> move (20,-42)
  , rect 18 21 |> filled (hsl (degrees 176) 0.928 0.94)
    |> move (20,-42)
  , oval 14 20 |> filled yellow
    |> move (19, -42.4)
  , rect 14 8 |> filled yellow
    |> move (19, -48.5)
  , roundedRect 10.5 4.5 1|> filled black
    |> move (22, -40)
  , roundedRect 10 4 1|> filled (hsl (degrees 185) 0.853 0.745)
    |> move (22, -40)
  , rect 18 21 |> filled (hsl (degrees 176) 0.928 0.94)
    |> move (20,-42)
    |> makeTransparent 0.7
  , pocket
  , pocket 
    |> move (0,-5)
  , pocket
    |> move (-2,-15)
    |> scale 0.9
  , rect 56 24
    |> filled gray
    |> makeTransparent 0.875
    |> move (-90, 40)
  , text "Admin: "
    |> filled black
    |> scale 0.5
    |> move (-95, 45)
  , text "Card Swipe "
    |> filled black
    |> scale 0.5
    |> move (-95, 35)
  --, text (String.fromFloat model.swipeTime)
  --  |> filled red
  , text (model.fail1)
    |> sansserif
    |> outlined (dashed 0.2) white
    |> move (-72,87)
    |> scale 0.5
    |> makeTransparent (2*model.swipeTime)
  , text (model.fail2)
    |> sansserif
    |> outlined (dashed 0.2) white
    |> move (-72,87)
    |> scale 0.5
  , text (model.success)
    |> sansserif
    |> outlined (dashed 0.2) white
    |> move (-72,87)
    |> scale 0.5
  , text "Please Swipe Card"
    |> sansserif
    |> outlined (dashed 0.2) white
    |> move (-72,87)
    |> scale 0.5
    |> makeTransparent (2 - timedata.time)
  ]
  ++
  ( case model.state of 
      Waiting -> []
      Dragging _ ->
        [rect 190 126 |> filled (rgba 0 0 0 0.01)
          |> notifyMouseMoveAt (\x -> (Core.AMsg (MovePos x)))
          |> notifyMouseUp (Core.AMsg LetGo)
          |> notifyLeave (Core.AMsg LetGo)
        ]
  ) ++  ( if (model.succeeded) then
            [
                group
                  [
                      roundedRect 25 14 2
                        |> filled green
                        |> addOutline (solid 1) blue
                    , text "Next"
                        |> size 8
                        |> centered
                        |> filled blue
                        |> move (0,-3)
                  ] |> move (75,-50)
                    |> notifyTap (Core.Shared (Core.Next 1))
            ]
          else
            []
        )

sub (x,y) (u,v) = (x-u,y-v)
add (x,y) (u,v) = (x+u,y+v) 

pocket = group
    [ curve (0,0) [Pull (10,0) (20,-10)]
      |> filled black
      |> rotate (degrees 26)
      |> scale 1.5
      |> scaleX 1.1
      |> move (-40, -37)
    , curve (0,0) [Pull (10,0) (20,-10)]
      |> filled (hsl (degrees 16) 0.936 0.34)
      |> rotate (degrees 26)
      |> scale 1.5
      |> scaleX 1.1
      |> move (-40, -38)
    ]

card = group
    [ roundedRect 68 38 2|> filled white
    , rect 20.5 25.5 |> filled black
      |> move (-20,0)
    , rect 20 25 |> filled (hsl (degrees 0) 0.726 0.918)
      |> move (-20,0)
    , oval 14 20 |> filled red
      |> move (-17, -2.9)
    , rect 14 8 |> filled red
      |> move (-17, -8.5)
    , roundedRect 10.5 4.5 1|> filled black
      |> move (-20, 2)
    , roundedRect 10 4 1|> filled (hsl (degrees 185) 0.853 0.745)
      |> move (-20, 2)
    , text "Crewmate"
      |> filled black
      |> scale 0.5
      |> move (-1, 9)
    , rect 30 1 |> filled black
      |> move (12, 5)
    , rect 30 1 |> filled black
      |> move (12, 0)
    , rect 30 1 |> filled black
      |> move (12, -5)
    ]

smallgray colour = group
         [rect 110 10 |> filled colour
            |> move (0,4)
         , roundedRect 100 10 5 |> filled colour
            |> move (-5,10)
         , rect 100 10 |> filled colour
            |> move (5,10)
         ]

biggray colour = group
            [rect 110 20 |> filled colour
              |> move (0,43)
            , roundedRect 100 25 5 |> filled colour
              |> move (-5,34)
            , rect 100 25 |> filled colour
              |> move (5,34)
            ]
    
  
