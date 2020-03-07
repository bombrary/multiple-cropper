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


port askClippedImage : JE.Value -> Cmd msg


port receiveClippedImage : (JE.Value -> msg) -> Sub msg


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
    | ClippedImageReceived JE.Value
    | AddBox
    | NameChanged BB.Id String
    | Download


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boxies =
            BB.fromList
                [ BB.bboxOrigin ( 0, -0.7 ) ( 201, 47 )
                , BB.bboxOrigin ( 5, 187.6 ) ( 307, 252.3 )
                , BB.bboxOrigin ( 48, 350 ) ( 199, 490 )
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
            ( newModel
            , Cmd.none
            )

        DragEnded ->
            ( { model
                | boxies = BB.toggleHold Nothing model.boxies
              }
            , clippedHeldImageCmd model
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
            let
                newModel =
                    setImage model value
            in
            ( newModel
            , allClippedImageCmd newModel
            )

        ClippedImageReceived value ->
            ( setClippedImage model value
            , Cmd.none
            )

        AddBox ->
            let
                newModel =
                    addBox model

                id =
                    model.boxies.nextId
            in
            ( newModel
            , clippedImageCmd id newModel
            )

        NameChanged id newName ->
            ( setNewName id newName model
            , Cmd.none
            )

        Download ->
            ( model
            , Download.bytes "images.zip"
                "application/zip"
                (BB.toZip model.boxies)
            )


setNewName : BB.Id -> String -> Model -> Model
setNewName id newName ({ boxies } as model) =
    let
        newBoxies =
            BB.update id (\b -> { b | name = newName }) boxies
    in
    { model | boxies = newBoxies }


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


type alias ClippedImageProposal =
    { src : String
    , id : Int
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


clippedImageProposal : ClippedImageProposal -> JE.Value
clippedImageProposal { src, id, x, y, width, height } =
    JE.object
        [ ( "src", JE.string src )
        , ( "id", JE.int id )
        , ( "x", JE.float x )
        , ( "y", JE.float y )
        , ( "width", JE.float width )
        , ( "height", JE.float height )
        ]


clippedImageCmd : BB.Id -> Model -> Cmd Msg
clippedImageCmd id { boxies, image } =
    Maybe.withDefault Cmd.none <|
        Maybe.map2
            (\box { src, size } ->
                let
                    imgBox =
                        scaleForImg size box
                in
                askClippedImage <|
                    clippedImageProposal
                        { src = src
                        , id = id
                        , x = imgBox.s.x
                        , y = imgBox.s.y
                        , width = BB.width imgBox
                        , height = BB.height imgBox
                        }
            )
            (BB.get id boxies)
            (Result.toMaybe image)


clippedHeldImageCmd : Model -> Cmd Msg
clippedHeldImageCmd ({ boxies, image } as model) =
    case boxies.hold of
        Nothing ->
            Cmd.none

        Just { id } ->
            clippedImageCmd id model


allClippedImageCmd : Model -> Cmd Msg
allClippedImageCmd { boxies, image } =
    Result.withDefault Cmd.none <|
        Result.map
            (\{ src, size } ->
                Cmd.batch <|
                    BB.toListWith
                        (\_ id box ->
                            let
                                imgBox =
                                    scaleForImg size box
                            in
                            askClippedImage <|
                                clippedImageProposal
                                    { src = src
                                    , id = id
                                    , x = imgBox.s.x
                                    , y = imgBox.s.y
                                    , width = BB.width imgBox
                                    , height = BB.height imgBox
                                    }
                        )
                        boxies
            )
            image


addBox : Model -> Model
addBox ({ image } as model) =
    case image of
        Err _ ->
            model

        Ok { src, size } ->
            let
                l =
                    100

                c =
                    Vec (svgWidth size / 2) (svgHeight size / 2)

                s =
                    V.toTuple <| V.add c (Vec (-l / 2) (-l / 2))

                t =
                    V.toTuple <| V.add c (Vec (l / 2) (l / 2))

                newBox =
                    BB.bboxOrigin s t
            in
            { model | boxies = BB.add newBox model.boxies }


scaleForImg : Rect -> BB.BBox -> BB.BBox
scaleForImg rect ({ s, t } as bbox) =
    let
        r =
            rect.width / svgWidth rect
    in
    { bbox
        | s = V.scale r s
        , t = V.scale r t
    }


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


setClippedImage : Model -> JE.Value -> Model
setClippedImage ({ boxies } as model) value =
    let
        idAndSrc id src =
            { id = id, src = src }

        decoder =
            JD.map2 idAndSrc
                (JD.field "id" JD.int)
                (JD.field "src" JD.string)
    in
    case JD.decodeValue decoder value of
        Err _ ->
            model

        Ok { id, src } ->
            { model
                | boxies =
                    BB.update id
                        (\b -> { b | clippedImg = Just src })
                        model.boxies
            }



--- view


view : Model -> Html Msg
view model =
    div
        [ class "wrapper"
        ]
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
                , SA.class "main"
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
dMoveTo v =
    "M" ++ V.toString v


dLineTo : Vec -> String
dLineTo v =
    "L" ++ V.toString v


translate : Vec -> String
translate v =
    "translate(" ++ V.toString v ++ ")"



-- viewSide


viewSide : Model -> Html Msg
viewSide model =
    div
        [ class "side"
        ]
        [ button [ onClick ImageRequested ] [ text "Load Image" ]
        , button [ onClick AddBox ] [ text "Add" ]
        , viewClippedImages model
        , button [ onClick Download ] [ text "Download" ]
        ]



-- view for Debug


viewHeldBoxInfo : Model -> Html Msg
viewHeldBoxInfo model =
    case BB.getSelectedBox model.boxies of
        Nothing ->
            div [] []

        Just box ->
            div []
                [ p []
                    [ text <| "(" ++ V.toString box.s ++ ")" ]
                , p []
                    [ text <| "(" ++ V.toString box.t ++ ")" ]
                ]


viewClippedImages : Model -> Html Msg
viewClippedImages ({ boxies } as model) =
    div [ class "clipped-images" ] <|
        List.map
            (viewClippedImage model)
            (BB.toListWith Box boxies)


viewClippedImage : Model -> Box -> Html Msg
viewClippedImage model { id, bbox } =
    case bbox.clippedImg of
        Nothing ->
            p [] [ text "No Image" ]

        Just url ->
            div []
                [ img
                    [ src url
                    , style "width" "200px"
                    , style "height" "auto"
                    , style "border" "1px solid #333"
                    ]
                    []
                , input
                    [ onInput (NameChanged id)
                    , placeholder (String.fromInt id)
                    ]
                    []
                , span [] [ text ".png" ]
                ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveImageInfo ImageInfoReceived
        , BE.onMouseUp (JD.succeed DragEnded)
        , receiveClippedImage ClippedImageReceived
        ]
