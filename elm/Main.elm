module Main exposing (..)

import Fractals exposing (..)
import Html as H exposing (Html)
import Html.Events as Ev


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model = Model 1 kochkurve
        , update = update
        , view = view
        }


type alias Model =
    { iters : Int
    , fractal : Fractal
    }


type Msg
    = IncrIter
    | DecrIter


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrIter ->
            { model | iters = model.iters + 1 }

        DecrIter ->
            { model | iters = model.iters - 1 }


view : Model -> Html Msg
view model =
    H.div []
        [ H.div []
            [ H.button [ Ev.onClick DecrIter ] [ H.text "-" ]
            , H.button [ Ev.onClick IncrIter ] [ H.text "+" ]
            ]
        , genFractal model.fractal model.iters
        ]
