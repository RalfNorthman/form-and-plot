module Main exposing (main)

import Browser
import Element exposing (..)
import Browser exposing (sandbox)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


---- MODEL ----


type alias Model =
    { temperature : String
    , humidity : String
    , pressure : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" "", Cmd.none )



---- UPDATE ----


type Msg
    = InputTemp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTemp str ->
            ( { model | temperature = str }, Cmd.none )



---- VIEW ----


inputField : String -> String -> (String -> msg) -> Element msg
inputField modelPart label msg =
    Input.text
        [ width <| px 300 ]
        { onChange = (\x -> msg x)
        , text = modelPart
        , placeholder = Nothing
        , label = Input.labelAbove [ alignLeft ] <| text label
        }


view : Model -> Html Msg
view model =
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
    <|
        column
            [ spacing 10
            , padding 10
            ]
            [ text "Your Elm App is working!"
            , inputField model.temperature "Temperature [°C]" InputTemp
            , text model.temperature
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
