module Main exposing (main)

import Browser
import Element exposing (..)
import Browser exposing (sandbox)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Validator exposing (..)


---- MODEL ----


type alias Model =
    { temperature : Maybe Float
    , humidity : Maybe Float
    , pressure : Maybe Float
    , inTemp : String
    , inHumid : String
    , inPress : String
    , recentError : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Nothing Nothing "" "" "" False, Cmd.none )



---- VALIDATE ----


type TemperatureError
    = TemperatureNotNumberError
    | TemperatureBelowZeroKelvinError


type HumidityError
    = HumidityNotNumberError
    | HumidityBoundError


type PressureError
    = PressureNotNumberError
    | PressureBoundError


temperatureValidator : Validator (Maybe Float) TemperatureError
temperatureValidator =
    required TemperatureNotNumberError <|
        minBound TemperatureBelowZeroKelvinError -273.15


humidityValidator : Validator (Maybe Float) HumidityError
humidityValidator =
    required HumidityNotNumberError <|
        concat
            [ minBound HumidityBoundError 0
            , maxBound HumidityBoundError 100
            ]


pressureValidator : Validator (Maybe Float) PressureError
pressureValidator =
    required PressureNotNumberError <|
        concat
            [ minBound PressureBoundError 85
            , maxBound PressureBoundError 110
            ]


type FormError
    = TemperatureError TemperatureError
    | HumidityError HumidityError
    | PressureError PressureError


formValidator : Validator Model FormError
formValidator =
    concat
        [ liftMap TemperatureError .temperature temperatureValidator
        , liftMap HumidityError .humidity humidityValidator
        , liftMap PressureError .pressure pressureValidator
        ]


displayFormError : FormError -> String
displayFormError err =
    case err of
        TemperatureError tempErr ->
            case tempErr of
                TemperatureNotNumberError ->
                    "Temperature input needs a number."

                TemperatureBelowZeroKelvinError ->
                    "Temperature must be above -273.15°C."

        HumidityError humidErr ->
            case humidErr of
                HumidityNotNumberError ->
                    "Humidity input needs a number."

                HumidityBoundError ->
                    "Humidity must be between 0 and 100 percent."

        PressureError pressErr ->
            case pressErr of
                PressureNotNumberError ->
                    "Pressure input needs a number."

                PressureBoundError ->
                    "Pressure is usually between 85 and 110 kPa."



---- UPDATE ----


type Msg
    = InputTemp String
    | InputHumid String
    | InputPress String
    | ClickSubmit


commaToFloat : String -> Maybe Float
commaToFloat str =
    str
        |> String.replace "," "."
        |> String.toFloat


noErrors : Model -> Bool
noErrors model =
    errorList model |> List.isEmpty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTemp str ->
            ( { model
                | inTemp = str
                , temperature = commaToFloat str
                , recentError = False
              }
            , Cmd.none
            )

        InputHumid str ->
            ( { model
                | inHumid = str
                , humidity = commaToFloat str
                , recentError = False
              }
            , Cmd.none
            )

        InputPress str ->
            ( { model
                | inPress = str
                , pressure = commaToFloat str
                , recentError = False
              }
            , Cmd.none
            )

        ClickSubmit ->
            if noErrors model then
                ( model, Cmd.none )
            else
                ( { model
                    | recentError = True
                  }
                , Cmd.none
                )



---- COLORS ----


dangerRed =
    rgb 0.8 0.1 0.1


makeGrey number =
    rgb number number number


lightGrey =
    makeGrey 0.9


grey =
    makeGrey 0.8


darkGrey =
    makeGrey 0.7



---- VIEW  HELPERS ----


buttonStyle : List (Attribute Msg)
buttonStyle =
    [ Background.color lightGrey
    , Border.rounded 5
    , Border.width 2
    , Border.color darkGrey
    , mouseDown
        [ Background.color grey
        , scale <| 15 / 16
        ]
    , mouseOver [ scale <| 16 / 15 ]
    , padding 10
    ]



---- VIEW ----


inputField : String -> String -> (String -> msg) -> Element msg
inputField modelPart label msg =
    Input.text
        [ width <| px 200 ]
        { onChange = (\x -> msg x)
        , text = modelPart
        , placeholder = Nothing
        , label = Input.labelAbove [ alignLeft ] <| text label
        }


errorList : Model -> List FormError
errorList model =
    errors formValidator model


errorStringList : Model -> List String
errorStringList model =
    List.map displayFormError <| errorList model


displayErrors : Model -> Element msg
displayErrors model =
    let
        errorDisplay =
            column
                [ Font.color dangerRed
                , Font.alignLeft
                , spacing 5
                ]
            <|
                List.map text <|
                    errorStringList model
    in
        if model.recentError then
            errorDisplay
        else
            none


myButton : String -> Msg -> Element Msg
myButton label msg =
    Input.button
        buttonStyle
        { onPress = Just msg
        , label = text label
        }


myLayout : Element msg -> Html msg
myLayout element =
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
        element


view : Model -> Html Msg
view model =
    myLayout <|
        column
            [ spacing 10
            , padding 10
            , width <| px 300
            ]
            [ text "Your Elm App is working!"
            , inputField model.inTemp "Temperature [°C]" InputTemp
            , inputField model.inHumid "Humidity [%]" InputHumid
            , inputField model.inPress "Pressure [kPa]" InputPress
            , myButton "Submit" ClickSubmit
            , displayErrors model
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
