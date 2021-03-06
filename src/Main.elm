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
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Http
import HttpBuilder exposing (..)


---- FONT ----


googleFont : String -> Attribute Msg
googleFont fontName =
    let
        fontString =
            String.replace " " "+" fontName
    in
        Font.family
            [ Font.external
                { url =
                    "https://fonts.googleapis.com/css?family="
                        ++ fontString
                , name = fontName
                }
            ]



---- JSON ----


type alias Measurement =
    { id : Int
    , temperature : Float
    , humidity : Float
    , pressure : Float
    , comment : String
    }


decodeMeasurement : Decoder Measurement
decodeMeasurement =
    D.map5 Measurement
        (D.field "id" D.int)
        (D.field "temperature" D.float)
        (D.field "humidity" D.float)
        (D.field "pressure" D.float)
        (D.field "comment" D.string)


decodeMeasurementList : Decoder (List Measurement)
decodeMeasurementList =
    D.list decodeMeasurement


type alias NewMeasurement =
    { temperature : Float
    , humidity : Float
    , pressure : Float
    , comment : String
    }


encodeNewMeasurement : NewMeasurement -> Value
encodeNewMeasurement item =
    E.object
        [ ( "temperature", E.float item.temperature )
        , ( "humidity", E.float item.humidity )
        , ( "pressure", E.float item.pressure )
        , ( "comment", E.string item.comment )
        ]



---- HTTP ----


getAll : Cmd Msg
getAll =
    get "http://localhost:8000/measurements"
        |> withExpectJson decodeMeasurementList
        |> send requestAllHandler


requestAllHandler : Result Http.Error (List Measurement) -> Msg
requestAllHandler result =
    case result of
        Ok list ->
            GotAll list

        Err error ->
            RequestAllError error


postOne : NewMeasurement -> Cmd Msg
postOne item =
    post "http://localhost:8000/measurements"
        |> withJsonBody (encodeNewMeasurement item)
        |> send postOneHandler


postOneHandler : Result Http.Error () -> Msg
postOneHandler result =
    case result of
        Ok list ->
            PostSuccesful

        Err error ->
            PostOneError error



---- MODEL ----


type alias Model =
    { temperature : Maybe Float
    , humidity : Maybe Float
    , pressure : Maybe Float
    , inTemp : String
    , inHumid : String
    , inPress : String
    , inComment : String
    , warnings : Warnings
    , recent : Recent
    , requestStatus : RequestStatus
    , measurements : List Measurement
    }


init : ( Model, Cmd Msg )
init =
    ( { temperature = Nothing
      , humidity = Nothing
      , pressure = Nothing
      , inTemp = ""
      , inHumid = ""
      , inPress = ""
      , inComment = ""
      , warnings = HeedWarnings
      , recent = NoRecent
      , requestStatus = NotMadeYet
      , measurements = []
      }
    , getAll
    )


type RequestStatus
    = Okay
    | Error Http.Error
    | NotMadeYet
    | NoResponse


type Recent
    = RecentError
    | RecentWarning
    | NoRecent


type Warnings
    = HeedWarnings
    | IgnoreWarnings


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
        "Temperature [°C]:"
        InputTemp


humidity : Input HumidityError HumidityWarning
humidity =
    Input
        .humidity
        .inHumid
        humidityValidator
        humidityWarner
        "Humidity [%]:"
        InputHumid


pressure : Input PressureError PressureWarning
pressure =
    Input
        .pressure
        .inPress
        pressureValidator
        pressureWarner
        "Pressure [kPa]:"
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


areErrors : Model -> Bool
areErrors model =
    not <| List.isEmpty <| errorList model



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


areWarnings : Model -> Bool
areWarnings model =
    not <| List.isEmpty <| warningList model



---- UPDATE ----


type SubmitStatus
    = Errors
    | Warnings
    | Fine


submitStatus : Model -> SubmitStatus
submitStatus model =
    if areErrors model then
        Errors
    else if areWarnings model then
        Warnings
    else
        Fine


type Msg
    = InputTemp String
    | InputHumid String
    | InputPress String
    | InputComment String
    | ClickSubmit
    | Checkbox Bool
    | GotAll (List Measurement)
    | RequestAllError Http.Error
    | PostSuccesful
    | PostOneError Http.Error


convert : String -> Maybe Float
convert str =
    str
        |> String.replace "," "."
        |> String.toFloat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTemp str ->
            ( { model
                | inTemp = str
                , temperature = convert str
                , recent = NoRecent
              }
            , Cmd.none
            )

        InputHumid str ->
            ( { model
                | inHumid = str
                , humidity = convert str
                , recent = NoRecent
              }
            , Cmd.none
            )

        InputPress str ->
            ( { model
                | inPress = str
                , pressure = convert str
                , recent = NoRecent
              }
            , Cmd.none
            )

        InputComment str ->
            ( { model
                | inComment = str
                , recent = NoRecent
              }
            , Cmd.none
            )

        ClickSubmit ->
            let
                sendPost =
                    case
                        ( model.temperature
                        , model.humidity
                        , model.pressure
                        )
                    of
                        ( Just temp, Just humid, Just press ) ->
                            postOne <|
                                NewMeasurement
                                    temp
                                    humid
                                    press
                                    model.inComment

                        _ ->
                            Cmd.none
            in
                case
                    ( submitStatus model
                    , model.warnings
                    )
                of
                    ( Fine, _ ) ->
                        ( model, sendPost )

                    ( Warnings, IgnoreWarnings ) ->
                        ( model, sendPost )

                    ( Errors, _ ) ->
                        ( { model
                            | recent = RecentError
                          }
                        , Cmd.none
                        )

                    ( Warnings, HeedWarnings ) ->
                        ( { model
                            | recent = RecentWarning
                          }
                        , Cmd.none
                        )

        Checkbox bool ->
            ( { model
                | warnings =
                    if bool then
                        IgnoreWarnings
                    else
                        HeedWarnings
              }
            , Cmd.none
            )

        GotAll list ->
            ( { model
                | measurements = list
                , requestStatus = Okay
              }
            , Cmd.none
            )

        RequestAllError error ->
            ( { model
                | measurements = []
                , requestStatus = Error error
              }
            , Cmd.none
            )

        PostSuccesful ->
            init

        PostOneError error ->
            ( { model
                | measurements = []
                , requestStatus = Error error
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


white =
    makeGrey 1



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
                error =
                    not <| isValid input.validator <| input.getFloat model

                warn =
                    not <| isValid input.warner <| input.getFloat model

                borderColor =
                    case ( error, warn ) of
                        ( True, _ ) ->
                            lightRed

                        ( _, True ) ->
                            yellow

                        _ ->
                            grey

                borderWidth =
                    case ( model.recent, error || warn ) of
                        ( NoRecent, _ ) ->
                            1

                        ( _, False ) ->
                            1

                        _ ->
                            3
            in
                [ width <| px 200
                , Border.color borderColor
                , Border.width borderWidth
                , Border.rounded 5
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
    Color
    -> (Model -> List String)
    -> Model
    -> Element msg
display color listFunc model =
    let
        myText str =
            paragraph [] <| [ text str ]

        element =
            textColumn
                [ Font.color color
                , Font.alignLeft
                , width shrink
                , spacing 10
                ]
            <|
                List.map myText <|
                    listFunc model
    in
        if model.recent == NoRecent then
            none
        else
            element


displayErrors : Model -> Element msg
displayErrors =
    display
        textRed
        errorStringList


displayWarnings : Model -> Element msg
displayWarnings =
    display
        textYellow
        warningStringList


ignoreWarningsCheckbox : Model -> Element Msg
ignoreWarningsCheckbox model =
    let
        myIcon : Bool -> Element Msg
        myIcon checked =
            if checked then
                text "☑"
            else
                text "☐"

        checkbox =
            Input.checkbox []
                { onChange = \x -> Checkbox x
                , icon = myIcon
                , checked =
                    if model.warnings == IgnoreWarnings then
                        True
                    else
                        False
                , label =
                    Input.labelRight [] <|
                        text "Ignore warnings"
                }
    in
        case ( model.recent, model.warnings ) of
            ( RecentWarning, _ ) ->
                checkbox

            ( _, IgnoreWarnings ) ->
                checkbox

            _ ->
                none


myButton : String -> Msg -> Model -> Element Msg
myButton label msg model =
    let
        inactiveButton =
            el inactiveButtonStyle <| text label

        button =
            Input.button
                buttonStyle
                { onPress = Just msg
                , label = text label
                }
    in
        case ( model.recent, model.warnings ) of
            ( NoRecent, _ ) ->
                button

            ( RecentWarning, IgnoreWarnings ) ->
                button

            _ ->
                inactiveButton


myLayout : List (Attribute msg) -> Element msg -> Html msg
myLayout attributes element =
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        attributes
        element


commentInput : Model -> Element Msg
commentInput model =
    Input.text
        [ width <| px 200
        , Border.color grey
        , Border.width 1
        , Border.rounded 5
        ]
        { onChange = (\x -> InputComment x)
        , text = model.inComment
        , placeholder = Nothing
        , label = Input.labelAbove [ alignLeft ] <| text "Comment:"
        }


measurementCard : Measurement -> Element Msg
measurementCard item =
    row
        [ spacing 15
        ]
        [ text <| String.fromFloat item.temperature
        , text <| String.fromFloat item.humidity
        , text <| String.fromFloat item.pressure
        , text item.comment
        ]


inputs : Model -> Element Msg
inputs model =
    column
        [ spacing 20
        , padding 10
        , width <| px 300
        , alignTop
        , Border.color lightGrey
        , Border.width 1
        , Border.rounded 5
        ]
        [ inputField temperature model
        , inputField humidity model
        , inputField pressure model
        , commentInput model
        , myButton "Submit" ClickSubmit model
        , displayErrors model
        , displayWarnings model
        , ignoreWarningsCheckbox model
        ]


results : Model -> Element Msg
results model =
    column
        [ spacing 5
        , alignRight
        , alignTop
        , width fill
        , padding 10
        ]
    <|
        List.map measurementCard model.measurements


sillyTitle : Element Msg
sillyTitle =
    el
        [ Font.size 64
        , width fill
        , googleFont "Dokdo"
        , Background.color grey
        , Font.color white
        , padding 5
        , Border.rounded 20
        ]
    <|
        text "Form Input Stuff!"


view : Model -> Html Msg
view model =
    myLayout
        [ padding 20
        , googleFont "Montserrat"
        ]
    <|
        column
            [ width fill
            , spacing 20
            ]
            [ sillyTitle
            , row
                [ spacing 20
                , width fill
                ]
                [ inputs model
                , results model
                ]
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
