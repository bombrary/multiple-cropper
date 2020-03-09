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


port onKeyDown : (JE.Value -> msg) -> Sub msg


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
    , clippedImageRange : Range
    }


type alias Range =
    { s : Int
    , t : Int
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


type Key
    = Delete
    | Arrow Direction
    | Others String


type Direction
    = Up
    | Right
    | Down
    | Left


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
    | DuplicateBox
    | DeleteBox
    | NameChanged BB.Id String
    | Download
    | MoveBox Direction
    | KeyPressed Key
    | ChangeClippedImageRange Int


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
      , clippedImageRange = Range 0 3
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
            ( { model
                | boxies = BB.empty
                , clippedImageRange = Range 0 0
              }
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
                        |> updateClippedImageRange 0

                id =
                    model.boxies.nextId
            in
            ( newModel
            , clippedImageCmd id newModel
            )

        DuplicateBox ->
            let
                newModel =
                    copyBox model
                        |> updateClippedImageRange 0

                id =
                    model.boxies.nextId
            in
            ( newModel
            , clippedImageCmd id newModel
            )

        DeleteBox ->
            let
                newModel =
                    deleteBox model
                        |> updateClippedImageRange 0
            in
            ( newModel
            , Cmd.none
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

        MoveBox dir ->
            let
                newModel =
                    moveSelectedBox dir model

                cmd =
                    Maybe.withDefault Cmd.none <|
                        Maybe.map (\id -> clippedImageCmd id newModel)
                            model.boxies.select
            in
            ( moveSelectedBox dir model
            , cmd
            )

        KeyPressed key ->
            case key of
                Arrow dir ->
                    update (MoveBox dir) model

                Delete ->
                    update DeleteBox model

                Others _ ->
                    ( model, Cmd.none )

        ChangeClippedImageRange d ->
            ( updateClippedImageRange d model
            , Cmd.none
            )


updateClippedImageRange : Int -> Model -> Model
updateClippedImageRange d ({ boxies, clippedImageRange } as model) =
    let
        { s, t } =
            clippedImageRange

        len =
            BB.size boxies

        newCIR =
            if len <= 3 then
                Range 0 len

            else if 0 <= s + d && s + d + 3 <= len then
                Range (s + d) (s + d + 3)

            else
                Range s t

        _ =
            Debug.log "cir" newCIR
    in
    { model | clippedImageRange = newCIR }


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


copyBox : Model -> Model
copyBox ({ image, boxies } as model) =
    case BB.getSelectedBox boxies of
        Nothing ->
            model

        Just { s, t } ->
            let
                newBox =
                    BB.bboxOrigin
                        (V.toTuple s)
                        (V.toTuple t)
            in
            { model | boxies = BB.add newBox boxies }


deleteBox : Model -> Model
deleteBox ({ boxies } as model) =
    case boxies.select of
        Nothing ->
            model

        Just id ->
            { model | boxies = BB.remove id boxies }


moveSelectedBox : Direction -> Model -> Model
moveSelectedBox dir ({ boxies } as model) =
    case boxies.select of
        Nothing ->
            model

        Just id ->
            let
                move dx dy =
                    BB.update id
                        (BB.transform { x = 0, y = 0, dx = dx, dy = dy } BB.Inner)
                        boxies
            in
            case dir of
                Up ->
                    { model | boxies = move 0 -1 }

                Right ->
                    { model | boxies = move 1 0 }

                Down ->
                    { model | boxies = move 0 1 }

                Left ->
                    { model | boxies = move -1 0 }


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
    main_
        []
        [ viewSide model
        , viewMain model
        ]



-- viewMain


viewMain : Model -> Html Msg
viewMain ({ image } as model) =
    case image of
        Err info ->
            p [] [ text info ]

        Ok ({ src, size } as justImage) ->
            div
                [ tabindex 0
                , class "main"
                , id "main"
                ]
                [ S.svg
                    [ style "width" (String.fromFloat (svgWidth size))
                    , style "height" (String.fromFloat (svgHeight size))
                    , onMouseMove Dragged
                    , SA.class "main"
                    ]
                    [ viewImage justImage
                    , viewBoxies model
                    ]
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
            BB.toListWith makeBox boxies


makeBox : Bool -> BB.Id -> BB.BBox -> Box
makeBox isSelected id bbox =
    let
        w =
            BB.width bbox

        h =
            BB.height bbox
    in
    { isSelected = isSelected
    , id = id
    , bbox = bbox
    , width = w
    , height = h
    , r0 = Vec 0 0
    , r1 = Vec w 0
    , r2 = Vec w h
    , r3 = Vec 0 h
    }


type alias Box =
    { isSelected : Bool
    , id : BB.Id
    , bbox : BB.BBox
    , width : Float
    , height : Float
    , r0 : Vec
    , r1 : Vec
    , r2 : Vec
    , r3 : Vec
    }


viewBox : Model -> Box -> Svg Msg
viewBox model ({ bbox } as box) =
    S.g
        [ SA.transform <| translate bbox.s
        ]
        [ viewBoxBg model box
        , viewBoxLabel model box
        , viewBoxBody model box
        , viewBoxEdges model box
        , viewBoxCorners model box
        ]


viewBoxBg : Model -> Box -> Svg Msg
viewBoxBg model { id, bbox, r0, r1, r2, r3 } =
    S.path
        [ SA.d <| dMoveTo r0 ++ dLineTo r1 ++ dLineTo r2 ++ dLineTo r3 ++ " Z"
        , SA.fill "#333"
        , SA.opacity "0.3"
        , style "cursor" "move"
        ]
        []


viewBoxBody : Model -> Box -> Svg Msg
viewBoxBody model { id, bbox, r0, r1, r2, r3 } =
    S.path
        [ SA.d <| dMoveTo r0 ++ dLineTo r1 ++ dLineTo r2 ++ dLineTo r3 ++ " Z"
        , SA.fill "#f0f"
        , SA.opacity "0"
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
edgeViews ({ bbox, isSelected, r0, r1, r2, r3 } as box) =
    let
        color =
            if isSelected then
                "#f80"

            else
                "#333"
    in
    [ EdgeView r0 r1 color box BB.Above
    , EdgeView r1 r2 color box BB.Right
    , EdgeView r2 r3 color box BB.Below
    , EdgeView r3 r0 color box BB.Left
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


viewBoxLabel : Model -> Box -> Svg Msg
viewBoxLabel model { id, bbox } =
    case model.image of
        Err _ ->
            S.g [] []

        Ok { size } ->
            let
                width =
                    BB.width bbox

                height =
                    BB.height bbox

                r =
                    Basics.min width height / 2 - 10

                s =
                    if r < 0 then
                        0

                    else
                        0.5 * Basics.sqrt r
            in
            S.g
                [ SA.transform <| translate (Vec (width / 2) (height / 2))
                , SA.opacity "0.7"
                ]
                [ S.text_
                    [ SA.style "user-select: none;"
                    , SA.transform <| "scale(" ++ String.fromFloat s ++ ")"
                    , SA.textAnchor "middle"
                    , SA.dominantBaseline "central"
                    , SA.fill "white"
                    ]
                    [ S.text (String.fromInt id)
                    ]
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
cornerViews ({ isSelected, r0, r1, r2, r3 } as box) =
    let
        color =
            if isSelected then
                "#f80"

            else
                "#333"
    in
    [ CornerView r0 (Vec -2 -2) 4 color box BB.AboveLeft
    , CornerView r1 (Vec -2 -2) 4 color box BB.AboveRight
    , CornerView r2 (Vec -2 -2) 4 color box BB.BelowRight
    , CornerView r3 (Vec -2 -2) 4 color box BB.BelowLeft
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
        [ div
            [ class "side_btn side_btn-load"
            , onClick ImageRequested
            ]
            [ div [ class "side_btn-load_inner" ] []
            ]
        , div
            [ class "side_btn side_btn-add"
            , onClick AddBox
            ]
            [ div [ class "side_btn-add_inner" ] []
            ]
        , div
            [ class "side_btn side_btn-duplicate"
            , onClick DuplicateBox
            ]
            [ div [ class "side_btn-duplicate_inner" ] []
            ]
        , div
            [ class "side_btn side_btn-delete"
            , onClick DeleteBox
            ]
            [ div [ class "side_btn-delete_inner" ] []
            ]
        , viewClippedImages model
        , div
            [ class "side_btn side_btn-download"
            , onClick Download
            ]
            [ div
                [ class "side_btn-download_inner"
                ]
                [ div
                    [ class "side_btn-download_inner_bar"
                    ]
                    []
                ]
            ]
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
viewClippedImages ({ boxies, clippedImageRange } as model) =
    div [ class "side_img clippedImgContainer" ] <|
        [ button
            [ onClick (ChangeClippedImageRange -1)
            , class "clippedImgContainer_btn clippedImgContainer_btn-left"
            ]
            []
        , div [ class "clippedImgContainer_img clippedImgs" ] <|
            List.map
                (viewClippedImage model)
                (listSubseq clippedImageRange (BB.toListWith makeBox boxies))
        , button
            [ onClick (ChangeClippedImageRange 1)
            , class "clippedImgContainer_btn clippedImgContainer_btn-right"
            ]
            []
        ]


viewClippedImage : Model -> Box -> Html Msg
viewClippedImage model { id, bbox } =
    case bbox.clippedImg of
        Nothing ->
            p [] [ text "No Image" ]

        Just url ->
            div
                [ class "clippedImgs_img clippedImg"
                ]
                [ div
                    [ class "clippedImg_img"
                    ]
                    [ img
                        [ src url
                        ]
                        []
                    ]
                , div
                    [ class "clippedImg_filename"
                    ]
                    [ input
                        [ onInput (NameChanged id)
                        , placeholder (String.fromInt id)
                        , value bbox.name
                        ]
                        []
                    , span [] [ text ".png" ]
                    ]
                ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveImageInfo ImageInfoReceived
        , BE.onMouseUp (JD.succeed DragEnded)
        , receiveClippedImage ClippedImageReceived
        , onKeyDown keyDecode
        ]


keyDecode : JE.Value -> Msg
keyDecode value =
    Result.withDefault (KeyPressed (Others "undefined")) <|
        JD.decodeValue keyDecoder value


keyDecoder : JD.Decoder Msg
keyDecoder =
    JD.field "key" JD.string
        |> JD.map
            (\key ->
                case key of
                    "Delete" ->
                        KeyPressed Delete

                    "ArrowUp" ->
                        KeyPressed (Arrow Up)

                    "ArrowRight" ->
                        KeyPressed (Arrow Right)

                    "ArrowDown" ->
                        KeyPressed (Arrow Down)

                    "ArrowLeft" ->
                        KeyPressed (Arrow Left)

                    c ->
                        KeyPressed (Others c)
            )


listSubseq : Range -> List a -> List a
listSubseq { s, t } list =
    List.foldr
        (\( i, e ) acc ->
            if s <= i && i < t then
                e :: acc

            else
                acc
        )
        []
        (List.indexedMap Tuple.pair list)
