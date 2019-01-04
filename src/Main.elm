port module Main exposing (..)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attribs exposing (src, css, type_, value)
import Html.Styled.Events exposing (onCheck, onInput)
import Css exposing (..)
import Css.Global
import Dict exposing (Dict)
import FileSelect exposing (..)
import Styles
import File exposing (File)
import File.Download
import Task
import Csv
import Viz

read : File -> Cmd Msg
read file =
  Task.perform
    (FileContentLoaded (File.name file))
    (File.toString file)

---- MODEL ----


type alias Flags = Maybe Serialized

type alias Model =
  {
    meta : List String
  , fileSelect : FileSelect.Model
  , data : Data
  , selectedChannels : ChannelSelection
  }

initModel : Flags -> Model
initModel flags =
  let
    (data, selection) = case flags of
      Just serialized ->
        serialized
        |> Tuple.mapFirst Dict.fromList
        |> Tuple.mapSecond Dict.fromList
      Nothing -> (Dict.empty, Dict.empty)
  in
    Model
      []
      FileSelect.init
      data
      selection

type alias Data = Dict String CsvData

type alias MetaData = {
    samplingRate : Int
  , zeroSample : Int
  , xLabel : String
  , yLabel : String
  }

type alias Trial = List Channel

type alias Channel = {
    name : String
  , points : List Float
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags, Cmd.none )

type alias ChannelSelection = Dict String (Bool, String)

addChannel : String -> ChannelSelection -> ChannelSelection
addChannel channelName channelSelection =
  Dict.update channelName
  (\sel ->
    case sel of
      Just (True, color) -> Just (True, color)
      _ -> Just (False, defaultColor)
  )
  channelSelection

defaultColor = "#444"

---- UPDATE ----
type Msg
    = NoOp
    | FileSelectMsg FileSelect.Msg
    | FileContentLoaded String String
    | SetChannel String Bool String
    | Clear
    | DownloadButtonClicked
    | DownloadsReady (List String)


save : Model -> Cmd msg
save model =
  let
    data = (Dict.toList model.data)
    selection = Dict.toList model.selectedChannels
  in
    save_ (data, selection)

port save_ : Serialized -> Cmd msg
port clear_ : () -> Cmd msg
port download_ : List String -> Cmd msg

port startDownloads : (List String -> msg) -> Sub msg

type alias Serialized = (DataToSave, SelectionToSave)
type alias DataToSave = List (String, CsvData)
type alias SelectionToSave = List (String, (Bool, String))

fileNames = Dict.toList >> List.map Tuple.first

svgDownload name content =
  File.Download.string name "image/svg+xml" content

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Clear -> (initModel Nothing, clear_ ())
    DownloadButtonClicked -> (model, download_ (fileNames model.data))
    DownloadsReady contents ->
      let
        names =
          (fileNames model.data)
          |> List.map (\n -> String.split "." n |> String.join "-")
        downloads = List.map2 svgDownload names contents

      in
        (model, Cmd.batch downloads)
    FileSelectMsg msg_ ->
      let
        (model_, cmd) = FileSelect.update FileSelectMsg ["text/vhdr"] msg_ model.fileSelect
        cmds = Cmd.batch <| cmd :: List.map read model_.files
      in
        ( {model | fileSelect = model_}, cmds )
    NoOp -> (model, Cmd.none)
    SetChannel name val color ->
      let
        model_ = { model
          | selectedChannels = Dict.insert name (val, color) model.selectedChannels
          }
      in
        (model_, save model_)

    FileContentLoaded name content ->
      let
        model_ = case String.split "." name of
          [_, "txt"] ->
            let
              parsed = parseCsv content
            in
              { model
              | selectedChannels = addChannels parsed model.selectedChannels
              , data = Dict.insert name parsed model.data
              }

          [_, "vhdr"] ->
            let
              lines = String.split "\n" content
            in
              {model | meta = lines}

          [_, "vmrk"] ->
            let
              lines = String.split "\n" content
            in
              {model | meta = lines}
          _ -> model
      in
        (model_, save model_)

allChannels : CsvData -> List String
allChannels = List.map Tuple.first

addChannels :  CsvData -> ChannelSelection -> ChannelSelection
addChannels data selection =
  allChannels data
  |> List.foldl addChannel selection

parseRow : List String -> Maybe (String, (List String))
parseRow cells =
  case cells of
    "" :: [] -> Nothing
    head :: tail -> Just (head, tail)
    _ -> Nothing

parseCsv : String -> CsvData
parseCsv content =
    String.split "\n" content
      |> List.map (String.split ",")
      |> List.map parseRow
      |> List.filterMap identity

type alias CsvData =
  List (String, List String)

---- PROGRAM ----

main : Program Flags Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = always (startDownloads DownloadsReady)
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
    button [onClick (always DownloadButtonClicked)] [ text "download all" ]
  -- , globalStyles model
  , div [drawingSectionCss] [
      graph model
    , channelsPanel model.selectedChannels
    ]
  , meta model.meta
  , loadedData model.data
  , fileSelect model.fileSelect.draggedOver
  , button [onClick (always Clear)] [ text "clear all" ]
  ]

loadedData : Data -> Html msg
loadedData data=
  let
    names = Dict.toList data
      |> List.map Tuple.first
      |> List.map (\name -> div [] [text name])
  in
    div [] names

globalStyles : Model -> Html msg
globalStyles model =
  List.filterMap
    makeChannelStyles
    (Dict.toList model.selectedChannels)
  |> Css.Global.global


makeChannelStyles : (String, (Bool, String)) -> Maybe Css.Global.Snippet
makeChannelStyles (name, (active, color)) =
  case active of
    False -> Nothing
    True -> Just <| Css.Global.class name [
        property "stroke" color
      ]

graph : Model -> Html Msg
graph model =
  let
    selectedChannels = model.selectedChannels
      |> Dict.toList
      |> List.filter (Tuple.second >> Tuple.first)

    selectedChannelNames = selectedChannels
      |> List.map Tuple.first

    selectedChannelColors = selectedChannels
      |> List.map (Tuple.second >> Tuple.second)

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

    timelines =
      List.map
        (List.map2
          (\color (name, data) -> Viz.Timeline name color data)
          selectedChannelColors)
        lines

  in
    div [graphCss] [
      span [] [text "plotting for channels: "]
    , span [] [text <| String.join ", " selectedChannelNames]
    , div [] (List.map2 showVizForCondition conditionLabels timelines)
    ]

showVizForCondition label lines =
  div [css [padding (px 10)]] [
    div [css [padding3 (px 10) (px 0) (px 0), fontWeight bold]] [text label]
  , div [vizCss] [Viz.view label lines |> fromUnstyled]
  ]

vizCss = css [
    backgroundColor Styles.color.white
  ]
graphCss = css [
    width (pct 80)
  ]


channelsPanel : ChannelSelection -> Html Msg
channelsPanel channelSelection =
  div [] [
    h2 [] [text "Channel selection"]
  , div [] <| List.map channelControls (Dict.toList channelSelection)
  ]

channelControls : (String, (Bool, String)) -> Html Msg
channelControls (name, (selected, color)) =
  div [] [
    input [Attribs.id ("check-" ++ name), type_ "checkbox", Attribs.checked selected, onCheck (\checked -> SetChannel name checked color)] []
  , label [Attribs.for ("check-" ++ name)] [text name ]
  , if selected
    then input [type_ "color", value color, onInput (SetChannel name selected)] []
    else text ""
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
