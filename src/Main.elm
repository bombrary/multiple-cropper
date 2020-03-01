port module Main exposing (..)

import BBox as BB
import BBoxies as BB
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
import Vec as V exposing (Vec)


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
    { boxies : BB.BBoxies
    , image : Result String Image
    , mouse : Mouse
    }


type alias Mouse =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
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
    = DragStarted BB.HoldInfo
    | Dragged Float Float
    | DragEnded
    | ImageRequested
    | ImageSelected File
    | ImageEncoded String
    | ImageInfoReceived JE.Value


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boxies =
            BB.fromList
                [ BB.bboxOrigin ( 10, 20 ) ( 40, 100 )
                ]
      , image = Err "No Image"
      , mouse = Mouse 0 0 0 0
      }
    , askImageInfo "img/sample.png"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStarted holdInfo ->
            ( { model
                | boxies =
                    BB.toggleHold (Just holdInfo) model.boxies
                        |> BB.toggleSelect (Just holdInfo.id)
              }
            , Cmd.none
            )

        Dragged x y ->
            let
                newModel =
                    updateMouse x y model
                        |> updateHeldBox
            in
            ( newModel, Cmd.none )

        DragEnded ->
            ( { model
                | boxies = BB.toggleHold Nothing model.boxies
              }
            , Cmd.none
            )

        ImageRequested ->
            ( model
            , Select.file
                [ "image/bmp", "image/gif", "image/jpeg", "image/png" ]
                ImageSelected
            )

        ImageSelected file ->
            ( { model | boxies = BB.empty }
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


updateMouse : Float -> Float -> Model -> Model
updateMouse x y ({ mouse } as model) =
    let
        newMouse =
            { x = x
            , y = y
            , dx = x - mouse.x
            , dy = y - mouse.y
            }
    in
    { model | mouse = newMouse }


updateHeldBox : Model -> Model
updateHeldBox ({ mouse, boxies } as model) =
    { model | boxies = BB.updateHeldBox mouse boxies }


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
                , onMouseMove Dragged
                ]
                [ viewImage justImage
                , viewBoxies model
                ]


onMouseMove : (Float -> Float -> msg) -> S.Attribute msg
onMouseMove msg =
    SE.on "mousemove"
        (JD.map2 msg
            (JD.field "x" JD.float)
            (JD.field "y" JD.float)
        )


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


viewBoxies : Model -> Svg Msg
viewBoxies ({ boxies } as model) =
    S.g [ SA.class "boxies" ] <|
        List.map (viewBox model) <|
            BB.toListWith Box boxies


type alias Box =
    { isSelected : Bool
    , id : BB.Id
    , bbox : BB.BBox
    }


viewBox : Model -> Box -> Svg Msg
viewBox model box =
    S.g []
        [ viewBoxBody model box
        , viewBoxEdges model box
        , viewBoxCorners model box
        ]


viewBoxBody : Model -> Box -> Svg Msg
viewBoxBody model { id, bbox } =
    let
        { s, t } =
            bbox

        s_ =
            { x = t.x, y = s.y }

        t_ =
            { x = s.x, y = t.y }
    in
    S.path
        [ SA.d <| dMoveTo s ++ dLineTo s_ ++ dLineTo t ++ dLineTo t_ ++ " Z"
        , SA.fill "#333"
        , SA.opacity "0.3"
        , SE.onMouseDown <|
            DragStarted (BB.HoldInfo id BB.Inner)
        , style "cursor" "move"
        ]
        []


type alias EdgeView =
    { from : Vec
    , to : Vec
    , color : String
    , parent : Box
    , anchor : BB.Anchor
    }


edgeViews : Box -> List EdgeView
edgeViews ({ bbox, isSelected } as box) =
    let
        { s, t } =
            bbox

        s_ =
            { x = t.x, y = s.y }

        t_ =
            { x = s.x, y = t.y }

        color =
            if isSelected then
                "#f80"

            else
                "#333"
    in
    [ EdgeView s s_ color box BB.Above
    , EdgeView s_ t color box BB.Right
    , EdgeView t t_ color box BB.Below
    , EdgeView t_ s color box BB.Left
    ]


viewBoxEdges : Model -> Box -> Svg Msg
viewBoxEdges model box =
    S.g [ SA.class "edges" ] <|
        List.map (viewBoxEdge model) (edgeViews box)


viewBoxEdge : Model -> EdgeView -> Svg Msg
viewBoxEdge model { from, to, color, anchor, parent } =
    S.g [ SA.class "edge" ]
        [ S.path
            [ SA.d (dMoveTo from ++ " " ++ dLineTo to)
            , SA.strokeWidth "4"
            , SA.fill "none"
            , SA.stroke color
            , SE.onMouseDown <|
                DragStarted (BB.HoldInfo parent.id anchor)
            , styleCursor anchor
            ]
            []
        ]


type alias CornerView =
    { pos : Vec
    , diff : Vec
    , l : Float
    , color : String
    , parent : Box
    , anchor : BB.Anchor
    }


cornerViews : Box -> List CornerView
cornerViews ({ isSelected, bbox } as box) =
    let
        { s, t } =
            bbox

        s_ =
            { x = t.x, y = s.y }

        t_ =
            { x = s.x, y = t.y }

        color =
            if isSelected then
                "#f80"

            else
                "#333"
    in
    [ CornerView s (Vec -2 -2) 4 color box BB.AboveLeft
    , CornerView s_ (Vec -2 -2) 4 color box BB.AboveRight
    , CornerView t (Vec -2 -2) 4 color box BB.BelowRight
    , CornerView t_ (Vec -2 -2) 4 color box BB.BelowLeft
    ]


viewBoxCorner : Model -> CornerView -> Svg Msg
viewBoxCorner model { pos, diff, l, color, parent, anchor } =
    S.rect
        [ SA.transform (translate diff)
        , SA.x (String.fromFloat pos.x)
        , SA.y (String.fromFloat pos.y)
        , SA.fill color
        , SA.width (String.fromFloat l)
        , SA.height (String.fromFloat l)
        , SE.onMouseDown <|
            DragStarted (BB.HoldInfo parent.id anchor)
        , styleCursor anchor
        ]
        []


viewBoxCorners : Model -> Box -> Svg Msg
viewBoxCorners model box =
    S.g [ SA.class "corners" ] <|
        List.map (viewBoxCorner model) (cornerViews box)


styleCursor : BB.Anchor -> S.Attribute Msg
styleCursor anchor =
    let
        cursor =
            case anchor of
                BB.Inner ->
                    "move"

                BB.Above ->
                    "ns-resize"

                BB.Below ->
                    "ns-resize"

                BB.Left ->
                    "ew-resize"

                BB.Right ->
                    "ew-resize"

                BB.AboveLeft ->
                    "nwse-resize"

                BB.BelowRight ->
                    "nwse-resize"

                BB.AboveRight ->
                    "nesw-resize"

                BB.BelowLeft ->
                    "nesw-resize"
    in
    style "cursor" cursor


dMoveTo : Vec -> String
dMoveTo { x, y } =
    "M" ++ String.fromFloat x ++ "," ++ String.fromFloat y


dLineTo : Vec -> String
dLineTo { x, y } =
    "L" ++ String.fromFloat x ++ "," ++ String.fromFloat y


translate : Vec -> String
translate { x, y } =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"



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
        , BE.onMouseUp (JD.succeed DragEnded)
        ]
