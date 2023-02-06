module Kam exposing (shapes,update,Msg(..),Model,init)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core
import Bitwise

type Msg = ButtonPress Int

type alias Model = { correctPassword : List (List Int)
                    ,userPassword : List Int
                   }

init : Model
init = {correctPassword = [[8],[8,6],[8,6,0],[8,6,0,6],[8,6,0,6,4]]
       ,userPassword = []}


update : (Core.CoreMsg a Msg c d e) -> (Core.TimeData,Model) -> (Core.TimeData,Model)
update msg (timedata,model) = case msg of
  Core.DMsg a -> case a of
    ButtonPress idx -> (timedata,{ model | userPassword = model.userPassword ++ [idx]})
  _ -> (timedata,model)


button : Int -> Model -> Shape (Core.CoreMsg a Msg c d e)
button idx model =
  let
    x = modBy 3 idx |> toFloat
    y = idx // 3 |> toFloat
    fpos = ( x * 12 - 12, y * 12 - 14)
    color = if (List.member idx model.userPassword) then
                green -- probably green
            else
              hsl (degrees 354) 0.007 0.628
  in
  square 10
    |> filled color
    |> scale 1
    |> move fpos
    |> notifyTap (Core.DMsg (ButtonPress idx))

createButtons model = List.map (\x -> button x model) (List.range 0 8) |> group

shapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg a Msg c d e))
shapes (timedata,model) = [bg,
                          -- buttonReponse --
                          
                           [square 10
                             |> filled grey
                             |> scale 5
                            ,createButtons model
                           {- ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (0,10)
                           ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (-12,10)
                           ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (12,10)
                           ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (-12,-2)
                           ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (0,-2)
                           ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (12,-2)
                           ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (-12,-14)
                           ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (0,-14)
                           ,square 10
                             |> filled (hsl (degrees 354) 0.007 0.628)
                             |> scale 1
                             |> move (12,-14) -}
                           , circle 10
                             |> filled black
                             |> scale 0.2
                             |> move (-18,20)
                           ,circle 10
                             |> filled black
                             |> scale 0.2
                             |> move (-10,20)
                           ,circle 10 
                             |> filled black
                             |> scale 0.2
                             |> move (-1,20)
                           ,circle 10
                             |> filled black
                             |> scale 0.2
                             |> move (8,20)
                           ,circle 10
                             |> filled black
                             |> scale 0.2
                             |> move (17,20)
                           ,rect 10 20
                             |> filled (hsl (degrees 0) 0.002 0.331)
                             |> rotate (degrees 90)
                             |> scaleX 2
                             |> scaleY 0.4
                             |>move (0,-23)
                           ,triangle 10
                             |> filled (hsl (degrees 0) 0.002 0.331)
                             |> rotate (degrees 90)
                             |> scale 0.25
                             |> scaleX 2
                             |> scaleY 1
                             |> move (-20.1,-23.8)
                           ,triangle 10
                             |> filled (hsl (degrees 0) 0.002 0.331)
                             |> rotate (degrees 90)
                             |> scale 0.25
                             |> scaleX 2
                             |> scaleY 1
                             |> move (20.1,-23.8)
                           ]
                             |> group
                             |> move (32,0)
                             |> scale 1.5
                          ,

                        
                          -- buttonPattern --

                          [square 10
                            |> filled grey
                            |> scale 5
                           -- black bg of buttons --
                          ,square 10
                            |> filled black
                            |> scale 3.5
                            |> move (-0.5,-2)
                          -- actual buttons --
                          ,square 10
                            |> filled black
                            |> scale 1
                            |> move (0,10)
                          ,square 10
                            |> filled black
                            |> scale 1
                            |> move (-12,10)
                          ,square 10
                            |> filled black
                            |> scale 1
                            |> move (-12,-2)
                          ,square 10
                            |> filled black
                            |> scale 1
                            |> move (-12,-14)
                          ,square 10
                            |> filled black
                            |> scale 1
                            |> move (0,-14)
                          ,square 10
                            |> filled black
                            |> scale 1
                            |> move (12,-14)
                          ,circle 10
                            |> filled black
                            |> scale 0.2
                            |> move (-18,20)
                          ,circle 10
                            |> filled black
                            |> scale 0.2
                            |> move (-10,20)
                          ,circle 10
                            |> filled black
                            |> scale 0.2
                            |> move (-1,20)
                          ,circle 10
                            |> filled black
                            |> scale 0.2
                            |> move (8,20)
                          ,circle 10
                            |> filled black
                            |> scale 0.2
                            |> move (17,20)
                          ,rect 10 20
                            |> filled (hsl (degrees 0) 0.002 0.331)
                            |> rotate (degrees 90)
                            |> scaleX 2
                            |> scaleY 0.4
                            |> move (0,-23)
                          ,triangle 10
                            |> filled (hsl (degrees 0) 0.002 0.331)
                            |> rotate (degrees 90)
                            |> scale 0.25
                            |> scaleX 2
                            |> scaleY 1
                            |> move (-20.1,-23.8)
                          ,triangle 10
                            |> filled (hsl (degrees 0) 0.002 0.331)
                            |> rotate (degrees 90)
                            |> scale 0.25
                            |> scaleX 2
                            |> scaleY 1
                            |> move (20.1,-23.8)

                          ]
                            |> group
                            |> move (-30,0)
                            |> scale 1.5
                        ]

   

bg = rectangle 10 20
      |> filled (hsl (degrees 354) 0.007 0.467)  
      |> rotate (degrees 90)
      |> scale 15