module Fractals exposing (..)

import Svg exposing (Svg)
import Turtle
import Lindenmayer as L


type alias Fractal =
    { name : String
    , lSystem : L.LSys
    , turtleInterpreter : Int -> Char -> Maybe Turtle.Cmd
    , turtleStartPos : ( Float, Float )
    }


genFractal : Fractal -> Int -> Svg svg
genFractal fractal iters =
    fractal.lSystem
        |> L.execute (fractal.turtleInterpreter iters) iters
        |> \cmds ->
            Turtle.jump fractal.turtleStartPos
                :: cmds
                |> Turtle.runTurtle


kochkurve : Fractal
kochkurve =
    let
        interpreter n =
            let
                d =
                    100.0 / (3.0 ^ toFloat n)
            in
                \c ->
                    case c of
                        'F' ->
                            Just <| Turtle.move d

                        '+' ->
                            Just <| Turtle.turn 90.0

                        '-' ->
                            Just <| Turtle.turn -90.0

                        _ ->
                            Nothing

        rules =
            L.startWith "F"
                |> L.addRule 'F' "F+F-F-F+F"
    in
        Fractal "Kochkurve" rules interpreter ( -50.0, 25.0 )


snowFlake : Fractal
snowFlake =
    let
        interpreter n =
            let
                d =
                    100 / 3.0 ^ (toFloat n)
            in
                \c ->
                    case c of
                        'F' ->
                            Just <| Turtle.move d

                        '+' ->
                            Just <| Turtle.turn 60.0

                        '-' ->
                            Just <| Turtle.turn -60.0

                        _ ->
                            Nothing

        rules =
            L.startWith "Y"
                |> L.addRule 'F' "F+F--F+F"
                |> L.addRule 'Y' "F+F+F+F+F+F"
    in
        Fractal "Schneeflocke" rules interpreter ( -20.0, 30.0 )
