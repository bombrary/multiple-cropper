port module Main exposing (..)

import BBox as BB exposing (BBox)
import BBoxies as BB exposing (BBoxies)
import Browser
import Browser.Events as BE
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Task


port askImageInfo : String -> Cmd msg


port receiveImageInfo : (JE.Value -> msg) -> Sub msg


svgWidth : Rect -> Float
svgWidth rect =
    500


svgHeight : Rect -> Float
svgHeight { width, height } =
    height * 500 / width


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { boxies : BBoxies
    , image : Result String Image
    }


type alias Image =
    { src : String
    , size : Rect
    }


type alias Rect =
    { width : Float
    , height : Float
    }


type Msg
    = DragStarted
    | Dragged
    | DragEnded
    | ImageRequested
    | ImageSelected File
    | ImageEncoded String
    | ImageInfoReceived JE.Value


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boxies =
            BB.fromList
                [ BB.bbox ( 0, -0.68 ) ( 201, 46.8 )
                , BB.bbox ( 6, 188.32 ) ( 308, 65.58 )
                , BB.bbox ( 50, 352 ) ( 146, 135 )
                ]
      , image = Err "No Image"
      }
    , askImageInfo "img/sample.png"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStarted ->
            ( model, Cmd.none )

        Dragged ->
            ( model, Cmd.none )

        DragEnded ->
            ( model, Cmd.none )

        ImageRequested ->
            ( model
            , Select.file
                [ "image/bmp", "image/gif", "image/jpeg", "image/png" ]
                ImageSelected
            )

        ImageSelected file ->
            ( model
            , Task.perform ImageEncoded (File.toUrl file)
            )

        ImageEncoded url ->
            ( model
            , askImageInfo url
            )

        ImageInfoReceived value ->
            ( setImage model value
            , Cmd.none
            )


setImage : Model -> JE.Value -> Model
setImage model value =
    let
        makeImage src width height =
            { src = src
            , size =
                { width = width
                , height = height
                }
            }

        decoder =
            JD.map3 makeImage
                (JD.field "src" JD.string)
                (JD.field "width" JD.float)
                (JD.field "height" JD.float)

        image =
            case JD.decodeValue decoder value of
                Err err ->
                    Err (JD.errorToString err)

                Ok img ->
                    Ok img
    in
    { model | image = image }



--- view


view : Model -> Html Msg
view model =
    div []
        [ viewMain model
        , viewSide model
        ]



-- viewMain


viewMain : Model -> Html Msg
viewMain ({ image } as model) =
    case image of
        Err info ->
            p [] [ text info ]

        Ok ({ src, size } as justImage) ->
            S.svg
                [ style "width" (String.fromFloat (svgWidth size))
                , style "height" (String.fromFloat (svgHeight size))
                , style "border" "1px solid #000"
                ]
                [ viewImage justImage
                , viewBBoxies model
                ]


viewImage : Image -> Html Msg
viewImage { src, size } =
    S.image
        [ SA.x "0"
        , SA.y "0"
        , SA.width (String.fromFloat (svgWidth size))
        , SA.height (String.fromFloat (svgHeight size))
        , SA.xlinkHref src
        ]
        []


viewBBoxies : Model -> Svg Msg
viewBBoxies model =
    S.g [ SA.class "boxies" ] <|
        List.map (viewBBox model) <|
            List.map Tuple.second (BB.toList model.boxies)


viewBBox : Model -> BBox -> Svg Msg
viewBBox model box =
    S.g [] []



-- viewSide


viewSide : Model -> Html Msg
viewSide model =
    div []
        [ button [ onClick ImageRequested ] [ text "Load Image" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveImageInfo ImageInfoReceived
        ]
