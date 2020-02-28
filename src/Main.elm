module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- MODEL ----


type alias Model =
    { io : { input : String, output : Int, outputSecond : Int } }


init : ( Model, Cmd Msg )
init =
    ( { io = { input = "", output = 0, outputSecond = 0 } }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangeField String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeField newvalue ->
            let
                oldIo =
                    model.io

                newIo =
                    { oldIo
                        | input = newvalue
                        , output = hashOne 199 newvalue
                        , outputSecond = hashTwo 128 199 newvalue
                    }
            in
            ( { model | io = newIo }, Cmd.none )



---- OTHERS ----


hashOne : Int -> String -> Int
hashOne div str =
    str
        |> String.toList
        |> List.map Char.toCode
        |> List.foldl (\r l -> modBy div (modBy div l * modBy div r)) 1


hashTwo : Int -> Int -> String -> Int
hashTwo base div str =
    str
        |> String.toList
        |> List.map Char.toCode
        |> List.foldl (\r l -> modBy div (modBy div l * base * modBy div r)) 1



--|> modBy div
---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Your string here", value model.io.input, onInput ChangeField ] []
        , p [] []
        , text (String.fromInt model.io.output)
        , p [] []
        , text (String.fromInt model.io.outputSecond)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
