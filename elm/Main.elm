module Main exposing (..)

import Fractals exposing (..)
import Html as H exposing (Html, Attribute)
import Html.Events as Ev


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model =
            Model 1
                [ kochkurve, snowFlake, sierpinski, farn ]
                kochkurve
                Image
        , update = update
        , view = view
        }


type alias Model =
    { iters : Int
    , fractals : List Fractal
    , selected : Fractal
    , display : Display
    }


type Display
    = Image
    | Text


type Msg
    = IncrIter
    | DecrIter
    | FractalChanged (Maybe Fractal)
    | ToggleDisplay Display


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

        ToggleDisplay display ->
            { model | display = display }


view : Model -> Html Msg
view model =
    H.div []
        [ H.div []
            [ viewDisplayToggle model
            , H.button [ Ev.onClick DecrIter ] [ H.text "-" ]
            , H.button [ Ev.onClick IncrIter ] [ H.text "+" ]
            , H.label [] [ H.text <| "Iterations: " ++ toString model.iters ]
            ]
        , H.div
            []
            [ viewSelect model ]
        , viewRules model
        , viewFractal model
        ]


viewDisplayToggle : Model -> Html Msg
viewDisplayToggle model =
    case model.display of
        Image ->
            H.button [ Ev.onClick <| ToggleDisplay Text ] [ H.text " -> text " ]

        Text ->
            H.button [ Ev.onClick <| ToggleDisplay Image ] [ H.text " -> img " ]


viewFractal : Model -> Html Msg
viewFractal model =
    case model.display of
        Image ->
            genFractal model.selected model.iters

        Text ->
            genText model.selected model.iters
                |> (H.text >> List.singleton >> H.p [])


viewRules : Model -> Html Msg
viewRules model =
    H.ul []
        (model.selected
            |> showFractalRules
            |> List.map (H.text >> List.singleton >> H.li [])
        )


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
