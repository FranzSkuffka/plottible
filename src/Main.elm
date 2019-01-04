port module Main exposing (..)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attribs exposing (src, css, type_, value)
import Html.Styled.Events exposing (onCheck)
import Css exposing (..)
import Dict exposing (Dict)
import FileSelect exposing (..)
import Styles
import File exposing (File)
import Task
import Csv
import Viz

read : File -> Cmd Msg
read file =
  Task.perform
    (FileContentLoaded (File.name file))
    (File.toString file)

---- MODEL ----


type alias Model =
  {
    params : Params
  , meta : List String
  , fileSelect : FileSelect.Model
  , data : Dict String CsvData
  }

type alias Params = {
    range : (Int, Int)
  , channels : Dict String Bool
  }

type alias Trial = List Channel

type alias Channel = {
    name : String
  , points : List Float
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags, Cmd.none )

initModel flags =
  let
    csvData = Dict.fromList flags
  in
    Model
      (initParams flags)
      []
      FileSelect.init
      csvData

initParams : Flags -> Params
initParams flags =
  let
    channels_ = List.head flags
      |> Maybe.map Tuple.second
      |> Maybe.map (List.map Tuple.first)
      |> Maybe.withDefault []
      |> List.map (\ch -> (ch, False))
      |> Dict.fromList
  in
    Params
      (-2000, 2000)
      <| channels_

addChannel : String -> Dict String Bool -> Dict String Bool
addChannel channelName channelSelection =
  Dict.update channelName
  (\sel ->
    case sel of
      Just True -> Just True
      _ -> Just False
  )
  channelSelection


---- UPDATE ----
type Msg
    = NoOp
    | FileSelectMsg FileSelect.Msg
    | FileContentLoaded String String
    | SelectChannel String Bool

port save : List (String, CsvData) -> Cmd msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FileSelectMsg msg_ ->
      let
        (model_, cmd) = FileSelect.update FileSelectMsg ["text/vhdr"] msg_ model.fileSelect
        cmds = Cmd.batch <| cmd :: List.map read model_.files
      in
        ( {model | fileSelect = model_}, cmds )
    NoOp -> (model, Cmd.none)
    SelectChannel name val ->
      let
        p = model.params
        params = {p | channels = Dict.insert name val p.channels}
        m =
          { model
          | params = params
          }
      in
        (m, Cmd.none)

    FileContentLoaded name content ->
      let
        model_ = case String.endsWith ".txt" name of
          True ->
            let
              p = model.params
              parsed = parse content
              allChannels = List.map Tuple.first parsed
              params = { p | channels = List.foldl addChannel p.channels allChannels}
              data = Dict.insert name parsed model.data
              m =
                { model
                -- | files = FileContent name content :: model.files
                | params = params
                , data = data
                }
            in
              m

          False ->
            case String.endsWith ".vhdr" name of
              False -> model
              True ->
                let
                  lines = String.split "\n" content
                in
                  {model | meta = lines}
      in
        (model_, save (Dict.toList model_.data))

parseRow : List String -> Maybe (String, (List String))
parseRow cells =
  case cells of
    "" :: [] -> Nothing
    head :: tail -> Just (head, tail)
    _ -> Nothing

parse : String -> CsvData
parse content =
    String.split "\n" content
      |> List.map (String.split ",")
      |> List.map parseRow
      |> List.filterMap identity

type alias CsvData =
  List (String, List String)

---- PROGRAM ----

type alias Flags = List (String, CsvData)

main : Program Flags Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

meta : List String -> Html Msg
meta lines = div [] <| List.map (\l -> div [] [text l])lines


drawingSectionCss =
  css [
    displayFlex
  , backgroundColor Styles.color.lightGrey
  ]

view : Model -> Html Msg
view model =
  div [] [
    div [drawingSectionCss] [
      graph model
    , channels model.params
    ]
  , meta model.meta
  , fileSelect model.fileSelect.draggedOver
  ]

graph : Model -> Html Msg
graph model =
  let
    selectedChannelNames = model.params.channels
      |> Dict.toList
      |> List.filter Tuple.second
      |> List.map Tuple.first
    conditions = Dict.toList model.data
    conditionLabels = List.map Tuple.first conditions

    channelsToPlot =
      List.map Tuple.second conditions -- only data of condition
      |> List.map -- each condition
        (List.filter -- each channel
          (\(name, data) ->
              List.member name selectedChannelNames
            )
        )

    lines =
        (List.map (List.map (Tuple.mapSecond (List.map String.toFloat))))
        channelsToPlot

  in
    div [graphCss] [
      span [] [text "plotting for channels: "]
    , span [] [text <| String.join ", " selectedChannelNames]
    , div [] (List.map2 showVizForCondition conditionLabels lines)
    ]

showVizForCondition label lines =
  div [css [padding (px 10)]] [
    div [css [padding3 (px 10) (px 0) (px 0), fontWeight bold]] [text label]
  , div [vizCss] [Viz.view lines |> fromUnstyled]
  ]

vizCss = css [
    backgroundColor Styles.color.white
  ]
graphCss = css [
    width (pct 80)
  ]


channels : Params -> Html Msg
channels params =
  div [] [
    h2 [] [text "Channel selection"]
  , div [] <| List.map channelCheckbox (Dict.toList params.channels)
  ]

channelCheckbox : (String, Bool) -> Html Msg
channelCheckbox (name, selected) =
  div [] [
    input [Attribs.id ("check-" ++ name), type_ "checkbox", Attribs.checked selected, onCheck (SelectChannel name)] []
  , label [Attribs.for ("check-" ++ name)] [text name ]
  ]

type alias FileContent = {
    name : String
  , content : String
  }

fileSelect : Bool -> Html Msg
fileSelect draggedOver =
  let
    interactiveAttribs = FileSelect.onClick FileSelectMsg :: FileSelect.onDrop FileSelectMsg
  in
    div
      (fileSelectorCss draggedOver :: interactiveAttribs)
      [text "Click to select or drop text or vhdr files here"]

fileSelectorCss : Bool -> Attribute msg
fileSelectorCss draggedOver = css <| [
    backgroundColor Styles.color.lightGrey
  , color Styles.color.black
  , padding (vw 10)
  , margin3 Styles.unit Styles.unit (px 0)
  , cursor pointer
  , hover highlightStyles
  , fontSize Styles.font.big
  , textAlign center
  ] ++ (if draggedOver then highlightStyles else [])

highlightStyles : List Style
highlightStyles = [
    backgroundColor Styles.color.lightBlue
  , color Styles.color.white
  ]
