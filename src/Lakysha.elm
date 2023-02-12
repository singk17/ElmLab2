module Lakysha exposing (shapes ,update,Msg(..),Model,init)

import Dict exposing (Dict)
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core
type State 
  = Waiting
  | Dragging


type alias Model = {time : Float
    , pos : (Float,Float)  -- circle centre if not draging, or the mouse click point if dragging
    , state : State 
    , letGoPos : (Float,Float)
    , startPos: (Float,Float)
    , connectedWires : List Int
    , selectedLine : Int
    , screen : Screen
    }
type Screen = Success | Game
    
type alias Wire = {
  startPos: (Float,Float)
  ,endPos:(Float,Float)
  ,colour:Color
  , screen : Screen
    }

defaultWireList =[ 
    {
      startPos= (-50,30)
      ,endPos= (49,-30)
      ,colour=pink
    }
    ,

    {
      startPos= (-50,10)
      ,endPos= (49,-10)
      ,colour=yellow
    }
    ,
     {
      startPos= (-50,-10)
      ,endPos= (49,10)
      ,colour=blue
    }
    ,
    {
      startPos= (-50,-30)
      ,endPos= (49,30)
      ,colour=red
    }

  ]

getElement : List a -> Int -> a
getElement  li idx = case (List.drop idx li |> List.head) of 
  Just a -> a
  Nothing -> Debug.todo "index out of bounds"

init : Model
init = {time = 0, pos = (0,0), state = Waiting, letGoPos = (0,0), connectedWires = [], startPos = (0,0), selectedLine = 0, screen=Game}

type Msg 
  = Tick Float GetKeyState
  | NewPos Int (Float,Float)
  | MovePos (Float,Float)
  | LetGo (Float,Float)
  | PlayAgain
  


wireStart idx = 
    let 
      wire=getElement defaultWireList idx
      (x,y)= wire.startPos
      (ex,ey)=wire.endPos
      o=6
    in
    group [
     rectangle 8 3
        |> filled wire.colour
        |> move (x-o,y)
        |> notifyMouseDownAt (\p -> (Core.LMsg (NewPos idx p)))
    , 
     rectangle 8 3
        |> filled wire.colour
        |>move (ex+o,ey)
        
    ,
    connection  
      |>move(wire.startPos)
      |> notifyMouseDownAt (\p -> (Core.LMsg (NewPos idx p)))
    , connection
      |> mirrorX
      |> move wire.endPos
   ] 


connection=
   [
    rect 4 3
      |> filled (hsl (degrees 21) 0.537 0.467)
      |> rotate (degrees 180) 

   ]
   |>group

shapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg a b c Msg e))
shapes (timedata,model) =
  case model.screen of
  Success -> [ 
   square 300
      |> filled (gradient [stop  (hsl (degrees 281) 0.22 0.056) -50, stop (hsl (degrees 259) 0.002 0.191) 0.66, stop  (hsl (degrees 281) 0.22 0.056) 50])
      |>move(0,0)
      ,
      rect 4 150
      |> filled (hsl (degrees 52) 0.09 0.363)
      |> move (-75,0)
      ,
      rect 4 150
      |> filled (hsl (degrees 52) 0.09 0.363)
      |> move (75,0)
      ,
      text "Success" 
      |>sansserif
      |>filled white
      |>scale(2) 
      |>move(-45,0)
      

      ]
  Game ->
    let
      (x,y) = model.pos
    in
    [
        rectangle 500 700
          |>filled black
      
        ,
      
      square 300
      |> filled (gradient [stop  (hsl (degrees 281) 0.22 0.056) -50, stop (hsl (degrees 259) 0.002 0.191) 0.66, stop  (hsl (degrees 281) 0.22 0.056) 50])
      |>move(0,0)
      ,
      rect 4 150
      |> filled (hsl (degrees 52) 0.09 0.363)
      |> move (-75,0)
      ,
      rect 4 150
      |> filled (hsl (degrees 52) 0.09 0.363)
      |> move (75,0)
      
      , group( 
        List.map wireStart (List.range 0 3)
      )
     
    ] ++ (
            if (model.state == Dragging) then
              let 
                selectedLine=getElement defaultWireList model.selectedLine
              in 
                
              [
                  line model.pos model.startPos
                    |> outlined (solid 3) selectedLine.colour
                , rectangle 192 128
                    |> filled white
                    |> makeTransparent 0
                    |> notifyMouseMoveAt (\p -> (Core.LMsg (MovePos p)))
                    |> notifyMouseUpAt (\p -> Core.LMsg (LetGo p)) 
              ]
            else []
          ) ++ (List.map drawWire model.connectedWires)

drawWire idx = 
  let 
    selectedLine=getElement defaultWireList idx
  in  
    line selectedLine.startPos selectedLine.endPos
      |>outlined (solid 3) selectedLine.colour

update : (Core.CoreMsg a b c Msg e) -> (Core.TimeData,Model) -> (Core.TimeData,Model)
update msg (timedata,model) =
  case msg of
    Core.LMsg a -> case a of
      NewPos idx (x,y) -> 
        if (List.member idx model.connectedWires) then
          (timedata,model)
        else
          let 
            selectedLine=getElement defaultWireList idx
          in 
          (timedata,{model | pos = (x,y), state = Dragging, startPos = selectedLine.startPos, selectedLine = idx })
      MovePos (x,y) -> if (model.state == Dragging) then
                          (timedata,{model | pos = (x,y) })
                       else (timedata,model)
      LetGo (x,y) -> 
        let
          selectedLine=getElement defaultWireList model.selectedLine
          (ex,ey)=selectedLine.endPos
        in
          if (abs(ex-x)<2.5 && abs(ey-y)<2.5) then
            let
              newConnectedWires =  model.connectedWires ++ [ model.selectedLine ]
            in
            if (List.length newConnectedWires == List.length defaultWireList) then 
             Debug.log "done" (timedata ,{model | state = Waiting, screen=Success, connectedWires = newConnectedWires})
            else
             Debug.log "next" (timedata,{model | state = Waiting, letGoPos=(x,y), connectedWires = newConnectedWires  })
          else 
            Debug.log "fail" (timedata,{model | state = Waiting})
      _ -> (timedata,model)
    _ -> (timedata,model)

-- |> notifyTap (Core.Shared (Core.Next 1))