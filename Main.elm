module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Color exposing (..)
import Element exposing (toHtml, show)
import Collage exposing (..)
import Random
import Random.Color
import Time exposing (..)
import Array exposing (..)

-- import Components.Panel

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type NeighbourType = Moore | Neumann

type alias Rule =
    { range: Int
    , threshold: Int
    , count: Int
    , neighbour: NeighbourType
    }

type alias Model =
    { width: Int
    , height: Int
    , size: Int
    , cells: List (List Int)
    , colors: List Color
    , rule: Rule
    }

type Msg =
    NoOp |
    Update |
    UpdateRandom (List (List Int)) |
    SwitchTo NeighbourType |
    UpdateRange String |
    UpdateThreshold String |
    UpdateCount String |
    Randomize

initModel : Model
initModel =
    { width = 500
    , height = 500
    , size = 10
    , cells = 0 |> List.repeat 50 |> List.repeat 50
    , colors = initColors
    , rule = Rule 1 1 1 Neumann
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

getNextCellValue : Rule -> Int -> Int -> Int -> Array (Array Int) -> Int
getNextCellValue rule x y currentValue cellArray =
    let
        nextValue = (currentValue + 1) % rule.count
        neighbors = getNeighbors x y cellArray
        count = neighbors |> List.filter(\v -> v == nextValue) |> List.length
    in
        if count >= rule.threshold then
            nextValue
        else
            currentValue

updateCells : Rule -> List (List Int) -> List (List Int)
updateCells rule cells =
    let cellArray = cells
                       |> List.map(\row -> row |> Array.fromList)
                       |> Array.fromList
    in
        cells
            |> List.indexedMap(\y row -> row
                              |> List.indexedMap(\x cell -> getNextCellValue rule x y cell cellArray))

randomizeCells : Random.Generator (List (List Int))
randomizeCells =
    Random.list 50 (Random.int 0 15)
        |> Random.list 50

-- Update
toInt : String -> Int
toInt str =
    case String.toInt str of
        Ok val -> val
        Err msg -> 0

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRandom cells ->
            ( Model model.width model.height model.size cells model.colors model.rule, Cmd.none )
        Update ->
            ( Model model.width model.height model.size (updateCells model.rule model.cells) model.colors model.rule, Cmd.none )
        UpdateRange range ->
            let rule = Rule (toInt range) model.rule.threshold model.rule.count model.rule.neighbour in
            ( Model model.width model.height model.size model.cells model.colors rule, Cmd.none )
        UpdateThreshold threshold ->
            let rule = Rule model.rule.range (toInt threshold) model.rule.count model.rule.neighbour in
            ( Model model.width model.height model.size model.cells model.colors rule, Cmd.none )
        UpdateCount count ->
            let rule = Rule model.rule.range model.rule.threshold (toInt count) model.rule.neighbour in
            ( Model model.width model.height model.size model.cells model.colors rule, Cmd.none )
        Randomize ->
            ( model, Random.generate UpdateRandom randomizeCells )
        _ -> ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.millisecond * 50) (always Update)


-- View
slider : String -> Int -> Int -> (String -> Msg) -> Int -> Html Msg
slider name_ min max msg val =
    label []
        [ input [ type_ "range"
                , Html.Attributes.min <| toString min
                , Html.Attributes.max <| toString max
                , value <| toString val
                , onInput msg ] []
        , Html.text <| name_ ++ " : " ++ (toString val)
        ]

radio : String -> String -> msg -> Html msg
radio name_ value msg =
    label []
        [ input [type_ "radio", name name_, onClick msg] []
        , Html.text value
        ]

panel : Rule -> Html Msg
panel rule =
    div [ id "p" ]
        [ button [ onClick Randomize ] [ Html.text "randomize" ]
        , div []
            [ radio "neighbor" "NM" (SwitchTo Moore)
            , radio "neighbor" "NN" (SwitchTo Neumann)
            ]
        , div [] [ slider "range" 1 10 UpdateRange rule.range ]
        , div [] [ slider "threshold" 0 30 UpdateThreshold rule.threshold ]
        , div [] [ slider "count" 0 16 UpdateCount rule.count ]
        ]

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
        s = toFloat model.size
        yoffset = (toFloat h / 2) - (s / 2)
        cells = model.cells
        colors = model.colors
    in
        div [] [
             panel model.rule
            , collage w h [ cells
                          |> List.indexedMap(\y line -> formLine s w ((toFloat y) * s - yoffset) colors line)
                          |> List.concat
                          |> group
                          ] |> toHtml
            ]
