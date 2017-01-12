module Main exposing (main)

import Html exposing (Html, program, div, text)
import Color exposing (..)
import Element exposing (toHtml, show)
import Collage exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Cell =
    { color: Color
    , x: Int
    , y: Int
    }

type alias Model =
    { width: Int
    , height: Int
    , size: Int
    , cells: List Cell
    }

type Msg = Update | Clear

initModel : Model
initModel =
    { width = 300
    , height = 300
    , size = 50
    , cells =
          [ Cell red 0 0
          , Cell blue 0 1
          , Cell green 0 2
          , Cell yellow 0 3
          , Cell black 0 4
          ]
    }

init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )

update : Msg -> Model -> ( Model, Cmd a )
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    let
        w = model.width
        h = model.height
        l = toFloat w / 2
        t = toFloat h / 2
        s = toFloat model.size
        cells = model.cells
        c = collage w h [
             group(List.map(\cell -> rect s s
                           |> filled cell.color
                           |> move ((toFloat cell.x) * s - l, (toFloat cell.y) * s - t)) cells)
            ]
    in
        toHtml c
