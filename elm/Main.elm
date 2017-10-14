module Main exposing (..)

import Fractals exposing (..)
import Html as H exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Decode as Json


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model = Model 1 [ kochkurve, snowFlake, sierpinski ] kochkurve
        , update = update
        , view = view
        }


type alias Model =
    { iters : Int
    , fractals : List Fractal
    , selected : Fractal
    }


type Msg
    = IncrIter
    | DecrIter
    | FractalChanged (Maybe Fractal)


findFractal : List Fractal -> String -> Maybe Fractal
findFractal fs name =
    fs
        |> List.filter (\f -> f.name == name)
        |> List.head


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrIter ->
            { model | iters = model.iters + 1 }

        DecrIter ->
            { model | iters = model.iters - 1 }

        FractalChanged Nothing ->
            model

        FractalChanged (Just f) ->
            { model
                | selected = f
                , iters = 1
            }


view : Model -> Html Msg
view model =
    H.div []
        [ H.div []
            [ H.button [ Ev.onClick DecrIter ] [ H.text "-" ]
            , H.button [ Ev.onClick IncrIter ] [ H.text "+" ]
            , H.label [] [ H.text <| "Iterations: " ++ toString model.iters ]
            ]
        , H.div []
            [ viewSelect model ]
        , genFractal model.selected model.iters
        ]


viewSelect : Model -> Html Msg
viewSelect model =
    let
        selKeyComp =
            model.selected.name

        genSelect item =
            H.button
                [ Ev.onClick (FractalChanged <| Just item)
                ]
                [ H.text <| item.name ]

        opts =
            List.map genSelect model.fractals
    in
        H.div [] opts
