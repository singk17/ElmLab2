module Dunya exposing (shapes,update,Msg(..),Model,init)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core

shapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg a Msg c d e))
shapes (timedata,model) = 
    case model.state of
        WelcomePage  ->
            [bgMain
            , startText
               |> move (0,40)
            ,astronautRed
               |> move (-35, 5)
               |> scale 0.5
               |> rotate (degrees (15 * timedata.time))
            , astronautBlue
               |> scale 0.5
               |> move (-50,10)
               |> rotate (degrees (-15 * timedata.time)) 
            , astronautOrange
               |> scale 0.5
               |> move (35,-5)
               |> rotate (degrees (20 * timedata.time))
            , astronautRed
               |> scale 0.5
               |> move (-60,20)
               |> rotate (degrees (15 * timedata.time))
            , astronautBlue
               |> scale 0.5
               |> move (45,-15)
               |> rotate (degrees (-15 * timedata.time))
            , astronautOrange
               |> scale 0.5
               |> move (-80,-20)
               |> rotate (degrees (10* timedata.time))

            , group
                  [
                       roundedRect 40 20 5
                            |> filled darkGray
                  ,    text "Play"
                            |> centered
                            |> size 8
                            |> filled white
                            |> move(0, -3)
                  ]
                     |> move (0, -25)
                     |> notifyTap (Core.DMsg WelcomeToCaf)
            ]

        Cafeteria  ->
            [ bgMain
            ,rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 4.5
                |> rotate (degrees 90)
                |> scale 1.25
            ,rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 1
                |> scale 1.25
                |> move (0,-12)
            , room
                |> scale 1.75
                |> move (65,5)
            ,text "Cafeteria"
                |> centered
                |> filled yellow
                |> scale 0.5
                |> move (65,10)
            ,astronautRed
                |> scale 0.25
                |> move (65,0)
            , group
                  [
                       room
                            |> scale 1.75
                  ,    text "Upper Engine"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(0, 6)
                  ]
                     |> move (-65, 5)
                     |> notifyTap (Core.DMsg CafToUppEng)
            , group
                  [
                       room
                            |> scale 1.75
                  ,    text "Med Bay"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(0, 6)
                  ]
                     |> move (0, -35)
                     |> notifyTap (Core.DMsg CafToMed)
            ]
        UpperEngine  ->
            [bgMain
            ,rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 4.5
                |> rotate (degrees 90)
                |> scale 1.25
                |> move (0,-30)
            ,rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 2.5
                |> scale 1
                |> move (0,-5)
            ,room
                |> scale 1.5
                |> move (0,35)
            ,text "Upper Engine"
                |> centered
                |> filled yellow
                |> scale 0.5
                |> move (0,35)
            ,astronautRed
                |> scale 0.25
                |> move (0,25)
            , group
                  [
                       room
                            |> move (-25,-5)
                            |> scale 1.5
                  ,    text "Reactor"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(-38, -2)
                  ]
                     |> move (-25, -25)
                     |> notifyTap (Core.DMsg UppEngTOReact)
            , group
                  [
                       room
                            |> move (25,-5)
                            |> scale 1.5
                  ,    text "Security"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(38, -2)
            
                  ]
                     |> move (25, -25)
                     |> notifyTap (Core.DMsg UppEngToSecurity)
            , group
                  [
                        room
                            |> scale 1.5
                            |> move (25,-20)
                  ,     text "Lower Engine"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move (25,-10)
                  ]
                     |> move (-25,-25)
                     |> notifyTap (Core.DMsg UppEngToLowEng)
            ]
        Reactor  ->
            [ bgMain
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 4.5
                |> rotate (degrees 90)
                |> scale 1.25
                |> move (0,20)
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 2.5
                |> scale 1
                |> move (2,-5)
            , room
                |> scale 1.75
                |> move (-65,20)
            , text "Reactor"
                |> centered
                |> filled yellow
                |> scale 0.5
                |> move (-65,30)
            , astronautRed
                |> scale 0.25
                |> move (-75,15)
            , group
                  [
                       circle 7
                            |> filled orange
                            |> move (-30,40)
                    ,  rect 10 10
                            |> filled yellow
                            |> scaleX 0.25
                            |> scale 0.3
                            |> scaleY 2.5
                            |> move (-30,42)
                    ,  circle 7
                            |> filled yellow
                            |> scale 0.1
                            |> move (-30,36)
                  ]
                     |> move (-25,-25)
                     |> notifyTap (Core.DMsg ReactToCleanUp)
            , group
                  [
                       room
                            |> scale 1.75
                            |> move (90,43)
                  ,    text "Security"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move (90,48)
                  ]
                     |> move (-25, -25)
                     |> notifyTap (Core.DMsg ReactToSecur)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (-22,-15)
                  ,    text "Lower Engine"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move (-22, -9)
                  ]
                     |> move (25, -25)
                     |> notifyTap (Core.DMsg ReactToLowerEngine)
            ]
        CleanUp  ->
            [ text "CleanUp"
                |> centered
                |> filled black
                |> scale 1
            , group
                  [
                    roundedRect 40 20 5
                            |> filled green
                  ,    text "BackToReactor"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-25,-25)
                     |> notifyTap (Core.DMsg CleanUpToReact)
            
            ]

        MedBay  ->
            [ bgMain
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 4.5
                |> rotate (degrees 90)
                |> scale 1
                |> move (-44,0)
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 1
                |> scale 1.25
                |> move (0,-12)
            , room
                  |> scale 1.75
                  |> move (0,-35)
            , text "MedBay"
                  |> centered
                  |> filled yellow
                  |> scale 0.5
                  |> move (0,-25)
            , astronautRed
                |> scale 0.25
                |> move (0,-35)
            , group
                  [
                       room
                            |> scale 1.75
                    
                  ,    text "Upper Engine"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(0, 6)
                  ]
                     |> move (-65,5)
                     |> notifyTap (Core.DMsg MedToUppEng)
            ]
        Security  ->
            [ bgMain
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 4.5
                |> rotate (degrees 90)
                |> scale 1.25
                |> move (0,20)
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 2.5
                |> scale 1
                |> move (2,-5)
            , room
                  |> scale 1.75
                  |> move (63,18)
            , text "Security"
                  |> centered
                  |> size 6
                  |> filled yellow
                  |> move (63,30)
            , astronautRed
                  |> scale 0.25
                  |> move (55,15)
            , group
                  [
                        circle 7
                            |> filled orange
                  ,     rect 10 10
                            |> filled yellow
                            |> scaleX 0.25
                            |> scale 0.3
                            |> scaleY 2.5
                            |> move (0,2)
                  ,     circle 7
                            |> filled yellow
                            |> scale 0.1
                            |> move (0,-4)     
                  ]
                     |> move (75,15)
                     |> notifyTap (Core.DMsg SecurToPassCode)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (-45,45)
                  ,    text "Reactor"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(-45,55)
                  ]
                     |> move (-25, -25)
                     |> notifyTap (Core.DMsg SecurToReact)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (-25,-15)
                  ,    text "Lower Engine"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move (-25, -10)
                  ]
                     |> move (25, -25)
                     |> notifyTap (Core.DMsg SecurToLowerEngine)
        
            ]
        PassCode  ->
            [ text "PassCodeGame"
                  |> centered
                  |> filled black
            , group
                  [
                        roundedRect 40 20 5
                            |> filled green
                  ,     text "PassCodeToSecurity"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move (0,-3)
                  ]
                     |> move (-25,-25)
                     |> notifyTap (Core.DMsg PassCodeToSecurity)

            ]

        LowerEngine  ->
            [bgMain
            ,rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 4.5
                |> rotate (degrees 90)
                |> scale 1.25
                |> move (0,20)
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 2.5
                |> scale 1
                |> move (2,-5)
            , room
                  |> scale 1.5
                  |> move (-60,25)
            , text "Lower Engine"
                  |> centered
                  |> size 6
                  |> filled yellow
                  |> move (-60,30)
            , astronautRed
                  |> scale 0.25
                  |> move (-60,20)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (25,-15)
                  ,    text "Electrical"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move (25, -10)
                  ]
                     |> move (-25, -25)
                     |> notifyTap (Core.DMsg LowerEngToElectric)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (30,50)
                  ,    text "Storage"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move (30, 55)
                  ]
                     |> move (25, -25)
                     |> notifyTap (Core.DMsg LowerEngToStorage)
            ]
        Electrical  ->
            [bgMain
            ,rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 4.5
                |> rotate (degrees 90)
                |> scale 1.25
                |> move (10,-28)
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 2.5
                |> scale 1
                |> move (-45,-5)
            , room
                  |> scale 1.75
                  |> move (-45,20)
            , text "Electrical"
                  |> centered
                  |> filled yellow
                  |> scale 0.5
                  |> move (-45,35)
            , astronautRed
                  |> scale 0.25
                  |> move (-55,15)
                  -- wires mini game --
            , group
                  [
                        circle 7
                            |> filled orange
                  ,     rect 10 10
                            |> filled yellow
                            |> scaleX 0.25
                            |> scale 0.3
                            |> scaleY 2.5
                            |> move (0,2)
                  ,     circle 7
                            |> filled yellow
                            |> scale 0.1
                            |> move (0,-4)     
                  ]
                     |> move (-30,17)
                     |> notifyTap (Core.DMsg ElectricalToWires)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (55,0)
                  ,    text "Storage"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(55, 10)
                  ]
                     |> move (0, -25)
                     |> notifyTap (Core.DMsg ElectricToStorage)
            ]
        Wires  ->
            [ text "Wires"
                  |> centered
                  |> filled black
            , group
                  [
                        roundedRect 40 20 5
                            |> filled green
                  ,     text "WiresToElectrical"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move (0,-3)
                  ]
                     |> move (-25,-25)
                     |> notifyTap (Core.DMsg WiresToElectrical)
            ]

        Storage  ->
            [bgMain
            ,rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 2
                |> rotate (degrees 90)
                |> scale 1.25
                |> move (25,-5)
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 2.5
                |> scale 1
                |> move (0,-5)
            , room
                  |> scale 1.5
                  |> move (0,-40)
            , astronautRed
                  |> scale 0.25
                  |> move (0,-45)
            , text "Storage"
                  |> centered
                  |> size 6
                  |> filled yellow
                  |> move (0,-35)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (85,20)
                  ,    text "Admin"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(85, 20)
                  ]
                     |> move (-25, -25)
                     |> notifyTap (Core.DMsg StorageToAdmin)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (-25,60)
                  ,    text "Cafeteria"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(-25, 60)
                  ]
                     |> move (25, -25)
                     |> notifyTap (Core.DMsg StorageToCafeteria)
            ]
        Admin  ->
            [ bgMain
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 3
                |> rotate (degrees 90)
                |> scale 1.25
                |> move (27,-5)
            , rect 10 20
                |> filled yellow
                |> scaleX 0.25
                |> scaleY 2.5
                |> scale 1
                |> move (-10,18)
            , room
                  |> scale 1.75
                  |> move (60,0)
            , astronautRed
                  |> scale 0.25
                  |> move (50,-5)
            , text "Admin"
                  |> centered
                  |> filled yellow
                  |> scale 0.5
                  |> move (60,10)
                  -- card swipe mini game --
            , group
                  [
                        circle 7
                            |> filled orange
                  ,     rect 10 10
                            |> filled yellow
                            |> scaleX 0.25
                            |> scale 0.3
                            |> scaleY 2.5
                            |> move (0,2)
                  ,     circle 7
                            |> filled yellow
                            |> scale 0.1
                            |> move (0,-4)   
                  ]
                     |> move (70,-3)
                     |> notifyTap (Core.DMsg AdminToCardSwipe)
            , group
                  [
                       room
                            |> scale 1.5
                            |> move (-10,60)
                  ,    text "Cafeteria"
                            |> centered
                            |> size 6
                            |> filled yellow
                            |> move(-10, 60)
                  ]
                     |> move (0, -25)
                     |> notifyTap (Core.DMsg AdminToCafeteria)
            ]
        CardSwipe  ->
            [ text "CardSwipe"
                  |> centered
                  |> filled black
            , group
                  [
                        roundedRect 40 20 5
                            |> filled green
                  ,     text "CardSwipeToAdmin"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move (0,-3)
                  ]
                     |> move (-25,-25)
                     |> notifyTap (Core.DMsg CardSwipeToAdmin)
                
            ]


startText =
    text "Among Us"
        |> bold
        |> fixedwidth
        |> centered
        |> filled green
        |> scale 1.5
        |> scaleX 1.2
        |> scaleY 1.2 


bgMain = 
    [roundedRect 10 20 5
        |> filled (hsl (degrees 237) 0.995 0.22)
        |> scale 5
        |> scaleX 15
        |> scaleY 5
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (-20,30)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (-65,35)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (65,35)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (15,30)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (15,55)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (-25,60)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (-25,-15)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (-30,0)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (-65,0)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (-75,-40)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (65,-40)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (45,10)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (75,-2)
    ,circle 7
        |> filled white
        |> scale 0.1
        |> move (0,-45)
    ]
    |> group

astronautRed = 
    [ roundedRect 10 20 5
        |> filled red
        |> scaleY 1.75
        |> scaleX 2.75
        |> scale 1
        |> move (0,2)
    , circle 7
        |> filled red
        |> scaleX 2.1
        |> scaleY 2
        |> move (0.5,15)
    , roundedRect 10 20 5
        |> filled red
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (-8.5,-12)
    , roundedRect 10 20 5
        |> filled red
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (8.5,-12)
    , roundedRect 10 20 5
        |> filled red
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (-13,0)
    , roundedRect 9 14 5
        |> filled (hsl (degrees 207) 0.995 0.832)
        |> scaleX 2
        |> scaleY 2
        |> rotate (degrees 90)
        |> scale 0.8
        |> move (6,17)
    , circle 7
        |> filled white
        |> scale 0.5
        |> scaleY 0.75
        |> scaleX 1.2
        |> move (10,18)
    ]
    |> group

astronautOrange = 
    [ roundedRect 10 20 5
        |> filled orange
        |> scaleY 1.75
        |> scaleX 2.75
        |> scale 1
        |> move (0,2)
    , circle 7
        |> filled orange
        |> scaleX 2.1
        |> scaleY 2
        |> move (0.5,15)
    , roundedRect 10 20 5
        |> filled orange
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (-8.5,-12)
    , roundedRect 10 20 5
        |> filled orange
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (8.5,-12)
    , roundedRect 10 20 5
        |> filled orange
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (-13,0)
    , roundedRect 9 14 5
        |> filled (hsl (degrees 207) 0.995 0.832)
        |> scaleX 2
        |> scaleY 2
        |> rotate (degrees 90)
        |> scale 0.8
        |> move (6,17)
    , circle 7
        |> filled white
        |> scale 0.5
        |> scaleY 0.75
        |> scaleX 1.2
        |> move (10,18)
    ]
    |> group

astronautBlue = 
    [ roundedRect 10 20 5
        |> filled blue
        |> scaleY 1.75
        |> scaleX 2.75
        |> scale 1
        |> move (0,2)
    , circle 7
        |> filled blue
        |> scaleX 2.1
        |> scaleY 2
        |> move (0.5,15)
    , roundedRect 10 20 5
        |> filled blue
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (-8.5,-12)
    , roundedRect 10 20 5
        |> filled blue
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (8.5,-12)
    , roundedRect 10 20 5
        |> filled blue
        |> scaleX 2
        |> scaleY 2
        |> scale 0.5
        |> move (-13,0)
    , roundedRect 9 14 5
        |> filled (hsl (degrees 207) 0.995 0.832)
        |> scaleX 2
        |> scaleY 2
        |> rotate (degrees 90)
        |> scale 0.8
        |> move (6,17)
    , circle 7
        |> filled white
        |> scale 0.5
        |> scaleY 0.75
        |> scaleX 1.2
        |> move (10,18)
    ]
    |> group


room = 
    roundedRect 13 14 2
        |> filled darkGray
        |> scaleX 2
        |> scaleY 2
        |> rotate (degrees 90)

type Msg = WelcomeToCaf 
         | CafToUppEng 
         | CafToMed 
         | UppEngTOReact 
         | ReactToSecur 
         | SecurToReact 
         | UppEngToSecurity 
         | UppEngToLowEng
         | SecurToLowerEngine 
         | SecurToPassCode
         | PassCodeToSecurity
         | ReactToLowerEngine
         | ReactToCleanUp
         | CleanUpToReact
         | LowerEngToElectric 
         | MedToUppEng 
         | LowerEngToStorage 
         | ElectricToStorage
         | ElectricalToWires
         | WiresToElectrical 
         | StorageToAdmin 
         | StorageToCafeteria 
         | AdminToCardSwipe
         | CardSwipeToAdmin
         | AdminToCafeteria 

type State = WelcomePage 
           | Cafeteria 
           | UpperEngine 
           | Reactor
           | CleanUp 
           | MedBay 
           | Security 
           | PassCode
           | LowerEngine 
           | Electrical 
           | Wires
           | Storage 
           | CardSwipe
           | Admin 


update : (Core.CoreMsg a Msg c d e) -> (Core.TimeData,Model) -> (Core.TimeData,Model)
update msg (timedata,model) =
  case msg of 
    Core.Shared a -> case a of 
        Core.Next b -> (timedata,model) -- if they win, this
        _ -> (timedata,model) -- if they lose, this
    Core.DMsg a -> case a of 
        WelcomeToCaf  ->
            case model.state of
                WelcomePage  ->
                    (timedata, { model | state = Cafeteria  })

                otherwise ->
                    (timedata,model)
        CafToUppEng  ->
            case model.state of
                Cafeteria  ->
                    (timedata, { model | state = UpperEngine  })

                otherwise ->
                    (timedata,model)
        CafToMed  ->
            case model.state of
                Cafeteria  ->
                    (timedata, { model | state = MedBay  })

                otherwise ->
                    (timedata,model)
        UppEngTOReact  ->
            case model.state of
                UpperEngine  ->
                    (timedata, { model | state = Reactor  })

                otherwise ->
                    (timedata,model)
        UppEngToLowEng  ->
            case model.state of
                UpperEngine  ->
                    (timedata, { model | state = LowerEngine  })
                
                otherwise ->
                    (timedata,model)
        ReactToSecur  ->
            case model.state of
                Reactor  ->
                    (timedata, { model | state = Security  })
                otherwise ->
                    (timedata,model)
        ReactToCleanUp  ->
            case model.state of
                Reactor  ->
                    (timedata, { model | state = CleanUp  })
                otherwise ->
                    (timedata,model)
        CleanUpToReact  ->
            case model.state of
                CleanUp  ->
                    (timedata, { model | state = Reactor  })
                otherwise ->
                    (timedata,model)        
        SecurToReact  ->
            case model.state of
                Security  ->
                    (timedata, { model | state = Reactor  })

                otherwise ->
                    (timedata,model)
        SecurToPassCode  ->
            case model.state of
                Security  ->
                    (timedata, { model | state = PassCode  })
                otherwise ->
                    (timedata,model)
        PassCodeToSecurity  ->
            case model.state of
                PassCode  ->
                    (timedata, { model | state = Security  })
                otherwise ->
                    (timedata,model)

        UppEngToSecurity  ->
            case model.state of
                UpperEngine  ->
                    (timedata, { model | state = Security  })

                otherwise ->
                    (timedata,model)
        SecurToLowerEngine  ->
            case model.state of
                Security  ->
                    (timedata, { model | state = LowerEngine  })

                otherwise ->
                    (timedata,model)
        ReactToLowerEngine  ->
            case model.state of
                Reactor  ->
                    (timedata, { model | state = LowerEngine  })

                otherwise ->
                    (timedata,model)
        LowerEngToElectric  ->
            case model.state of
                LowerEngine  ->
                    (timedata, { model | state = Electrical  })

                otherwise ->
                    (timedata,model)
        MedToUppEng  ->
            case model.state of
                MedBay  ->
                    (timedata, { model | state = UpperEngine  })

                otherwise ->
                    (timedata,model)
        LowerEngToStorage  ->
            case model.state of
                LowerEngine  ->
                    (timedata, { model | state = Storage  })

                otherwise ->
                    (timedata,model)
        ElectricToStorage  ->
            case model.state of
                Electrical  ->
                    (timedata, { model | state = Storage  })

                otherwise ->
                    (timedata,model)
        ElectricalToWires  ->
            case model.state of
                Electrical ->
                    (timedata, { model | state = Wires  })
                otherwise ->
                    (timedata,model)
        WiresToElectrical  ->
            case model.state of
                Wires ->
                    (timedata, { model | state = Electrical  })
                otherwise ->
                    (timedata,model)
        StorageToAdmin  ->
            case model.state of
                Storage  ->
                    (timedata, { model | state = Admin  })

                otherwise ->
                    (timedata,model)
        StorageToCafeteria  ->
            case model.state of
                Storage  ->
                    (timedata, { model | state = Cafeteria  })

                otherwise ->
                    (timedata,model)
        AdminToCardSwipe  ->
            case model.state of
                Admin ->
                    (timedata, { model | state = CardSwipe  })
                otherwise ->
                    (timedata,model)
        CardSwipeToAdmin  ->
            case model.state of
                CardSwipe ->
                    (timedata, { model | state = Admin  })
                otherwise ->
                    (timedata,model)
        AdminToCafeteria  ->
            case model.state of
                Admin  ->
                    (timedata, { model | state = Cafeteria  })

                otherwise ->
                    (timedata,model)
    _ -> (timedata,model)

type alias Model =
    { time : Float
    , state : State
    }

type alias Point = (Float, Float)

init : Model
init = { time = 0 
       , state = WelcomePage 
       }
    
    