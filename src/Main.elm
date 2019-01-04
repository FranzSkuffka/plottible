module Main exposing (..)

import Browser
import Html.Styled exposing (Html, text, div, h1, img, toUnstyled, Attribute)
import Html.Styled.Attributes exposing (src, css)
import Css exposing (..)
import Dict exposing (Dict)
import FileSelect exposing (..)
import Styles
import File exposing (File)


---- MODEL ----


type alias Model =
  {
    trials : List Trial
  , params : Params
  , files : Dict String File
  , fileSelect : FileSelect.Model
  }

type alias Params = {
    range : (Int, Int)
  , channels : Dict String String
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
    Dict.empty
    FileSelect.init

initParams =
  Params
    (-2000, 2000)
    Dict.empty


---- UPDATE ----


type Msg
    = NoOp
    | FileSelectMsg FileSelect.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FileSelectMsg msg_ ->
      let
        (model_, cmd) = FileSelect.update FileSelectMsg ["text/plain"] msg_ model.fileSelect
      in
        ( {model | fileSelect = model_}, cmd )
    NoOp -> (model, Cmd.none)



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
  , filesView model.files
  ]

filesView _ = text "Files here"
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

filesViewCss = css [
    padding Styles.unit
  ]

fileViewCss = css [
    marginBottom (px 10)
  , displayFlex
  ]

fileNameCss = css [
    fontWeight bold
  , padding2 (px 5) (px 10)
  , backgroundColor Styles.color.lightGrey
  ]
