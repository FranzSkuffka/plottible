module Main exposing (..)

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

read : File -> Cmd Msg
read file =
  Task.perform
    (FileContentLoaded (File.name file))
    (File.toString file)

---- MODEL ----


type alias Model =
  {
    trials : List Trial
  , params : Params
  , files : List FileContent
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

init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )

initModel =
  Model
    []
    initParams
    []
    FileSelect.init
    Dict.empty

initParams =
  Params
    (-2000, 2000)
    Dict.empty

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
        p = model.params
        parsed = parse content
        allChannels = List.map Tuple.first parsed
        params = case String.endsWith ".txt" name of
          True -> { p | channels = List.foldl addChannel p.channels allChannels}
          False -> p
        data = case String.endsWith ".txt" name of
          True -> Dict.insert name parsed model.data
          False -> model.data
        m =
          { model
          | files = FileContent name content :: model.files
          , params = params
          , data = data
          }
      in
        (m, Cmd.none)

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


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


view : Model -> Html Msg
view model =
  div [] [
    fileSelect model.fileSelect.draggedOver
  , filesView model
  , channels model.params
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

filesView : Model -> Html Msg
filesView model =
  div [] [
    h2 [] [text "loaded files"]
  , div [] <| List.map (\f -> div [] [File.name f |> text]) model.fileSelect.files
  , h2 [] [text "channels"]
  -- , channels model.files
  ]

type alias FileContent = {
    name : String
  , content : String
  }

fileStatusCss : Html.Styled.Attribute Msg
fileStatusCss =
  css [
    color (hex "FFF")
  , padding2 (px 5) (px 10)
  ]

fileSelect : Bool -> Html Msg
fileSelect draggedOver =
  let
    interactiveAttribs = FileSelect.onClick FileSelectMsg :: FileSelect.onDrop FileSelectMsg
  in
    div
      (fileSelectorCss draggedOver :: interactiveAttribs)
      [text "Click to select or drop WAV, AIF or AIFF files here"]

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
