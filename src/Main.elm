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
    , recentTrouble : Bool
    , ignoreWarnings : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Nothing Nothing "" "" "" False False, Cmd.none )


type alias Input err warn =
    { getFloat : Model -> Maybe Float
    , getString : Model -> String
    , validator : Validator (Maybe Float) err
    , warner : Validator (Maybe Float) warn
    , label : String
    , msg : String -> Msg
    }


temperature : Input TemperatureError TemperatureWarning
temperature =
    Input
        .temperature
        .inTemp
        temperatureValidator
        temperatureWarner
        "Temperature [°C]"
        InputTemp


humidity : Input HumidityError HumidityWarning
humidity =
    Input
        .humidity
        .inHumid
        humidityValidator
        humidityWarner
        "Humidity [%]"
        InputHumid


pressure : Input PressureError PressureWarning
pressure =
    Input
        .pressure
        .inPress
        pressureValidator
        pressureWarner
        "Pressure [kPa]"
        InputPress



---- VALIDATE ----


type TemperatureError
    = TemperatureNotNumberError
    | TemperatureBelowZeroKelvinError


type HumidityError
    = HumidityNotNumberError
    | HumidityBoundError


type PressureError
    = PressureNotNumberError
    | PressureUnderZeroError


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
        minBound PressureUnderZeroError 0


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

                PressureUnderZeroError ->
                    "Pressure must be above zero."


errorList : Model -> List FormError
errorList model =
    errors formValidator model


errorStringList : Model -> List String
errorStringList model =
    List.map displayFormError <| errorList model


noErrors : Model -> Bool
noErrors model =
    errorList model |> List.isEmpty



---- WARN ----


type TemperatureWarning
    = TemperatureBoundWarning


type HumidityWarning
    = None


type PressureWarning
    = PressureBoundWarning


temperatureWarner : Validator (Maybe Float) TemperatureWarning
temperatureWarner =
    optional <|
        concat
            [ minBound TemperatureBoundWarning -95
            , maxBound TemperatureBoundWarning 65
            ]


humidityWarner : Validator (Maybe Float) HumidityWarning
humidityWarner =
    succeed


pressureWarner : Validator (Maybe Float) PressureWarning
pressureWarner =
    optional <|
        concat
            [ minBound PressureBoundWarning 85
            , maxBound PressureBoundWarning 110
            ]


type FormWarning
    = TemperatureWarning TemperatureWarning
    | PressureWarning PressureWarning


formWarner : Validator Model FormWarning
formWarner =
    concat
        [ liftMap TemperatureWarning .temperature temperatureWarner
        , liftMap PressureWarning .pressure pressureWarner
        ]


displayFormWarning : FormWarning -> String
displayFormWarning warning =
    case warning of
        TemperatureWarning TemperatureBoundWarning ->
            "Temperature is usually between -95 and 65°C."

        PressureWarning PressureBoundWarning ->
            "Pressure is usually between 85 and 110 kPa."


warningList : Model -> List FormWarning
warningList model =
    errors formWarner model


warningStringList : Model -> List String
warningStringList model =
    List.map displayFormWarning <| warningList model


noWarnings : Model -> Bool
noWarnings model =
    warningList model |> List.isEmpty


areWarnings : Model -> Bool
areWarnings model =
    not <| noWarnings model



---- UPDATE ----


type Msg
    = InputTemp String
    | InputHumid String
    | InputPress String
    | ClickSubmit
    | Checkbox Bool


commaToFloat : String -> Maybe Float
commaToFloat str =
    str
        |> String.replace "," "."
        |> String.toFloat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTemp str ->
            ( { model
                | inTemp = str
                , temperature = commaToFloat str
                , recentTrouble = False
              }
            , Cmd.none
            )

        InputHumid str ->
            ( { model
                | inHumid = str
                , humidity = commaToFloat str
                , recentTrouble = False
              }
            , Cmd.none
            )

        InputPress str ->
            ( { model
                | inPress = str
                , pressure = commaToFloat str
                , recentTrouble = False
              }
            , Cmd.none
            )

        ClickSubmit ->
            case
                ( noErrors model
                , noWarnings model || model.ignoreWarnings
                )
            of
                ( True, True ) ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | recentTrouble = True
                      }
                    , Cmd.none
                    )

        Checkbox bool ->
            ( { model
                | ignoreWarnings = bool
              }
            , Cmd.none
            )



---- COLORS ----


textRed =
    rgb 0.8 0.1 0.1


lightRed =
    rgb 1 0.65 0.65


textYellow =
    rgb 0.8 0.7 0


yellow =
    rgb 1 0.95 0.15


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


inactiveButtonStyle : List (Attribute Msg)
inactiveButtonStyle =
    [ Background.color lightGrey
    , Border.rounded 5
    , Border.width 2
    , Border.color darkGrey
    , padding 10
    , alpha 0.4
    ]



---- VIEW ----


inputField : Input err warn -> Model -> Element Msg
inputField input model =
    let
        style =
            let
                valid =
                    isValid input.validator <| input.getFloat model

                warn =
                    not <| isValid input.warner <| input.getFloat model

                borderColor =
                    case ( valid, warn ) of
                        ( False, _ ) ->
                            lightRed

                        ( _, True ) ->
                            yellow

                        _ ->
                            grey

                borderWidth =
                    case ( valid, warn, model.recentTrouble ) of
                        ( False, _, True ) ->
                            3

                        ( _, True, True ) ->
                            3

                        _ ->
                            1
            in
                [ width <| px 200
                , Border.color borderColor
                , Border.width borderWidth
                ]
    in
        Input.text
            style
            { onChange = (\x -> input.msg x)
            , text = input.getString model
            , placeholder = Nothing
            , label = Input.labelAbove [ alignLeft ] <| text input.label
            }


display :
    Model
    -> Color
    -> (Model -> List String)
    -> Bool
    -> Element msg
display model color listFunc showing =
    let
        notifications =
            column
                [ Font.color color
                , Font.alignLeft
                , spacing 5
                ]
            <|
                List.map text <|
                    listFunc model
    in
        if showing then
            notifications
        else
            none


displayErrors : Model -> Element msg
displayErrors model =
    display model textRed errorStringList model.recentTrouble


displayWarnings : Model -> Element msg
displayWarnings model =
    display model textYellow warningStringList <|
        model.recentTrouble


ignoreWarningsCheckbox : Model -> Element Msg
ignoreWarningsCheckbox model =
    if
        model.ignoreWarnings
            || (noErrors model
                    && model.recentTrouble
                    && areWarnings model
               )
    then
        Input.checkbox []
            { onChange = \x -> Checkbox x
            , icon = Input.defaultCheckbox
            , checked = model.ignoreWarnings
            , label = Input.labelRight [] <| text "Submit despite warnings"
            }
    else
        none


myButton : String -> Msg -> Model -> Element Msg
myButton label msg model =
    if
        model.recentTrouble
            && not (model.ignoreWarnings && noErrors model)
    then
        el inactiveButtonStyle <| text label
    else
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
            , inputField temperature model
            , inputField humidity model
            , inputField pressure model
            , myButton "Submit" ClickSubmit model
            , displayErrors model
            , displayWarnings model
            , ignoreWarningsCheckbox model
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
