module Fractals exposing (..)

import Svg exposing (Svg)
import Turtle
import Lindenmayer as L


type alias Fractal =
    { name : String
    , lSystem : L.LSys
    , turtleInterpreter : Int -> Char -> Maybe Turtle.Cmd
    , iterAdjust : Int -> Int
    , turtleStartPos : ( Float, Float )
    }


genFractal : Fractal -> Int -> Svg svg
genFractal fractal iters =
    fractal.lSystem
        |> L.execute (fractal.turtleInterpreter iters) (fractal.iterAdjust iters)
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
        Fractal "Kochkurve" rules interpreter identity ( -50.0, 25.0 )


snowFlake : Fractal
snowFlake =
    let
        interpreter n =
            let
                d =
                    50 / (3.0 ^ toFloat n)
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
        Fractal "Schneeflocke" rules interpreter (\n -> n + 1) ( -25.0, 42.5 )


sierpinski : Fractal
sierpinski =
    let
        interpreter n =
            let
                d =
                    100.0 / (4.0 ^ toFloat n)
            in
                \c ->
                    case c of
                        'A' ->
                            Just <| Turtle.move d

                        'B' ->
                            Just <| Turtle.move d

                        '+' ->
                            Just <| Turtle.turn 60.0

                        '-' ->
                            Just <| Turtle.turn -60.0

                        _ ->
                            Nothing

        rules =
            L.startWith "A"
                |> L.addRule 'A' "B-A-B"
                |> L.addRule 'B' "A+B+A"
    in
        Fractal "Sierp. Dreieck" rules interpreter (\n -> n * 2) ( -50.0, 49.0 )


farn : Fractal
farn =
    let
        interpreter n =
            let
                d =
                    100.0 / (2.3 ^ toFloat n)
            in
                \c ->
                    case c of
                        'F' ->
                            Just <| Turtle.move d

                        '+' ->
                            Just <| Turtle.turn -25.0

                        '-' ->
                            Just <| Turtle.turn 25.0

                        '[' ->
                            Just <| Turtle.push

                        ']' ->
                            Just <| Turtle.pop

                        _ ->
                            Nothing

        rules =
            L.startWith "--X"
                |> L.addRule 'X' "F[-X][X]F[-X]+FX"
                |> L.addRule 'F' "FF"
    in
        Fractal "Farn" rules interpreter identity ( -50.0, 50.0 )
