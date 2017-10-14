module Turtle exposing (Distance, Angle, Cmd, jump, move, turn, pop, push, runTurtle)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Distance =
    Float


type alias Angle =
    Float


type Cmd
    = Jump ( Float, Float )
    | Move Distance
    | Turn Angle
    | Push
    | Pop


grad2rad : Float -> Float
grad2rad =
    let
        factor =
            2.0 * pi / 360.0
    in
        \grad -> grad * factor


jump : ( Float, Float ) -> Cmd
jump =
    Jump


move : Distance -> Cmd
move =
    Move


turn : Angle -> Cmd
turn angle =
    Turn (grad2rad angle)


push : Cmd
push =
    Push


pop : Cmd
pop =
    Pop


type alias State =
    { x : Float
    , y : Float
    , angle : Angle
    , dirX : Float
    , dirY : Float
    }


initState : State
initState =
    State 0.0 0.0 0.0 1.0 0.0


runTurtle : List Cmd -> Svg msg
runTurtle cmds =
    svg
        [ width "800", height "800", viewBox "-50 -50 100 100" ]
        (runCmds ( [], initState ) cmds |> (\( outs, _ ) -> outs))


runCmds : ( List State, State ) -> List Cmd -> ( List (Svg svg), ( List State, State ) )
runCmds state cmds =
    runCmdsAcc state cmds []


runCmdsAcc : ( List State, State ) -> List Cmd -> List (Svg svg) -> ( List (Svg svg), ( List State, State ) )
runCmdsAcc state cmds accSvg =
    case cmds of
        [] ->
            ( List.reverse accSvg, state )

        cmd :: cmds ->
            let
                ( out, state2 ) =
                    runCmd state cmd
            in
                case out of
                    Just someSvg ->
                        runCmdsAcc state2 cmds (someSvg :: accSvg)

                    Nothing ->
                        runCmdsAcc state2 cmds accSvg


runCmd : ( List State, State ) -> Cmd -> ( Maybe (Svg svg), ( List State, State ) )
runCmd ( stack, state ) cmd =
    case cmd of
        Push ->
            ( Nothing, ( state :: stack, state ) )

        Pop ->
            case stack of
                [] ->
                    ( Nothing, ( stack, state ) )

                state2 :: stack2 ->
                    ( Nothing, ( stack2, state2 ) )

        Jump ( x, y ) ->
            let
                state2 =
                    { state | x = x, y = y }
            in
                ( Nothing, ( stack, state2 ) )

        Move d ->
            let
                state2 =
                    { state
                        | x = state.x + d * state.dirX
                        , y = state.y + d * state.dirY
                    }
            in
                ( Just <|
                    line
                        [ x1 (toString state.x)
                        , y1 (toString state.y)
                        , x2 (toString state2.x)
                        , y2 (toString state2.y)
                        , stroke "rgb(0,0,0)"
                        , strokeWidth "0.1"
                        ]
                        []
                , ( stack, state2 )
                )

        Turn angle ->
            let
                angle2 =
                    state.angle + angle

                state2 =
                    { state
                        | dirX = cos angle2
                        , dirY = -1.0 * (sin angle2)
                        , angle = angle2
                    }
            in
                ( Nothing, ( stack, state2 ) )
