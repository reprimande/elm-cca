module Main exposing (main)

import Html exposing (Html, program, div, text)
import Color exposing (..)
import Element exposing (toHtml, show)
import Collage exposing (..)
import Random
import Random.Color
import Time exposing (..)
import Array exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- type Neighbourhood = Moore | Neumann

-- type alias Rule =
--     { range: Int
--     , threshold: Int
--     , count: Int
--     , neighbourhood: Neighbourhood
--     }

type alias Model =
    { width: Int
    , height: Int
    , size: Int
    , cells: List (List Int)
    , colors: List Color
    }

type Msg = Update | UpdateRandom (List (List Int))

initModel : Model
initModel =
    { width = 500
    , height = 500
    , size = 10
    , cells = 0 |> List.repeat 50 |> List.repeat 50
    , colors = initColors
    }

init : ( Model, Cmd Msg )
init =
    ( initModel, Random.generate UpdateRandom randomizeCells )

maxValue : Int
maxValue = 14

initColors : List Color
initColors =
  [ (255,0,0), (255,96,0), (255,191,0), (223,255,0), (128,255,0), (32,255,0), (0,255,64), (0,255,159), (0,255,255), (0,159,255), (0,64,255), (32,0,255), (127,0,255), (223,0,255) ]
      |> List.map(\(r,g,b) -> Color.rgb r g b)

getCellColor: List Color -> Int -> Color
getCellColor colors cellValue =
    let color = colors |> Array.fromList |> Array.get cellValue in
    case color of
        Just c -> c
        Nothing -> Color.rgb 0 0 0

getCoordValue : Int -> Int -> Int
getCoordValue max v =
    (v % max + max) % max

getCellValue : Int -> Int -> Array (Array Int) -> Int
getCellValue x y cellArray =
    let
        value = case (Array.get y cellArray) of
                    Just a -> Array.get x a
                    Nothing -> Nothing
    in
        case value of
            Just v -> v
            Nothing -> 0

getNeighbors : Int -> Int -> Array (Array Int) -> List Int
getNeighbors x y cellArray =
    [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]
        |> List.map(\(x, y) -> ((getCoordValue 50 x), (getCoordValue 50 y)))
        |> List.map(\(x, y) -> getCellValue x y cellArray)

getNextCellValue : Int -> Int -> Int -> Array (Array Int) -> Int
getNextCellValue x y currentValue cellArray =
    let
        nextValue = (currentValue + 1) % maxValue
        neighbors = getNeighbors x y cellArray
    in
        if List.member nextValue neighbors then
            nextValue
        else
            currentValue

updateCells : List (List Int) -> List (List Int)
updateCells cells =
    let cellArray =
            cells
                |> List.map(\row -> row |> Array.fromList)
                |> Array.fromList
    in
        cells
            |> List.indexedMap(\y row -> row
                              |> List.indexedMap(\x cell -> getNextCellValue x y cell cellArray))

randomizeCells : Random.Generator (List (List Int))
randomizeCells =
    Random.list 50 (Random.int 0 15)
        |> Random.list 50

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRandom cells -> (Model model.width model.height model.size cells model.colors, Cmd.none)
        Update -> (Model model.width model.height model.size (updateCells model.cells) model.colors, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.millisecond * 50) (always Update)

formLine : Float -> Int -> Float -> List Color -> List Int -> List Form
formLine size width top colors line =
    let
        offset = ((toFloat width) / 2) - (size / 2)
    in
        List.indexedMap(\x cell -> rect size size
                       |> filled (getCellColor colors cell)
                       |> move ((toFloat x) * size - offset, top)) line

view : Model -> Html Msg
view model =
    let
        w = model.width
        h = model.height
        l = toFloat w / 2
        t = toFloat h / 2
        s = toFloat model.size
        yoffset = t - (s / 2)
        cells = model.cells
        colors = model.colors
    in
        toHtml (collage w h [ group(List.concat (List.indexedMap(\y line -> formLine s w ((toFloat y) * s - yoffset) colors line) cells)) ])
