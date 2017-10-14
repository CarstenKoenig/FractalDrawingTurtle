module Main exposing (..)

import Svg exposing (Svg)
import Turtle
import Lindenmayer as L


main : Svg svg
main =
    Turtle.runTurtle (kochkurve 4)


kochkurve : Int -> List Turtle.Cmd
kochkurve n =
    let
        d =
            100.0 / (3.0 ^ toFloat n)

        interp c =
            case c of
                'F' ->
                    Just <| Turtle.move d

                '+' ->
                    Just <| Turtle.turn 90.0

                '-' ->
                    Just <| Turtle.turn -90.0

                _ ->
                    Nothing
    in
        L.startWith "F"
            |> L.addRule 'F' "F+F-F-F+F"
            |> L.execute interp n
            |> \cmds -> Turtle.jump ( -50.0, 25.0 ) :: cmds
