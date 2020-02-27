port module Main exposing (..)

import BBox as BB
import BBoxies as BB
import Base64
import Browser
import Browser.Events as BE
import Bytes
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
import Tar exposing (defaultMetadata)
import Task


port askClippedImages : JE.Value -> Cmd msg


port askImageSize : String -> Cmd msg


port receiveImageSize : (JE.Value -> msg) -> Sub msg


port receiveClippedImages : (JE.Value -> msg) -> Sub msg


port failedToLoadImage : (JE.Value -> msg) -> Sub msg


port preventDefaultOnKeyDown : (JE.Value -> msg) -> Sub msg


svgSizeWith : Float -> Float -> ( Float, Float )
svgSizeWith width height =
    ( 500
    , 500 * height / width
    )


svgSizeRatio : Float -> Float -> ( Float, Float )
svgSizeRatio width height =
    let
        ( w, h ) =
            svgSizeWith width height
    in
    ( width / w
    , height / h
    )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { boxies : BB.BBoxies
    , hold : HoldState
    , select : SelectState
    , mousePosition : MousePosition
    , imgWidth : Float
    , imgHeight : Float
    , imgSrc : String
    , clippedImages : List ClippedImage
    }


type alias ClippedImage =
    { url : String
    , name : String
    }


type HoldState
    = HoldBBox BB.Id BB.BBoxPosition
    | HoldNothing


type SelectState
    = SelectBBox BB.Id
    | SelectNothing


type alias MousePosition =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }


edgeW : Float
edgeW =
    5


initialBoxies : BB.BBoxies
initialBoxies =
    BB.fromList
        [ BB.BBox 0 -0.68 201 46.8 BB.None
        , BB.BBox 6 188.32 308 65.58 BB.None
        , BB.BBox 50 352 146 135 BB.None
        ]


init : JE.Value -> ( Model, Cmd Msg )
init jsVal =
    let
        src =
            Result.withDefault "" <|
                JD.decodeValue JD.string jsVal
    in
    ( { boxies = initialBoxies
      , hold = HoldNothing
      , select = SelectNothing
      , mousePosition = MousePosition 0 0 0 0
      , imgWidth = 1
      , imgHeight = 1
      , imgSrc = src
      , clippedImages = []
      }
    , askImageSize src
    )


type Msg
    = Hold BB.Id BB.BBoxPosition
    | MouseMove Float Float
    | Holding BB.Id BB.BBoxPosition
    | MouseUp BB.Id
    | KeyDowned Key
    | AddBox
    | CopyBox
    | ImageRequested
    | ImageSelected File
    | ImageUrlReceived String
    | ImageSizeReceived Float Float
    | ClippedImagesReceived (List String)
    | DownloadTar
    | ClippedImageNameChanged Int String
    | FailedToLoadImage


type Key
    = KeyDelete
    | KeyArrowLeft
    | KeyArrowUp
    | KeyArrowRight
    | KeyArrowDown
    | KeyOthers String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hold id pos ->
            ( { model
                | hold = HoldBBox id pos
                , select = SelectBBox id
              }
            , Cmd.none
            )

        MouseMove x y ->
            ( { model
                | mousePosition =
                    { x = x
                    , y = y
                    , dx = x - model.mousePosition.x
                    , dy = y - model.mousePosition.y
                    }
              }
            , Cmd.none
            )

        Holding id pos ->
            case BB.get id model.boxies of
                Nothing ->
                    ( model, Cmd.none )

                Just box ->
                    let
                        newBox =
                            BB.transform model.mousePosition pos box
                    in
                    ( { model
                        | boxies =
                            BB.insert id newBox model.boxies
                      }
                    , Cmd.none
                    )

        MouseUp id ->
            case BB.get id model.boxies of
                Nothing ->
                    ( model, Cmd.none )

                Just box ->
                    let
                        newBox =
                            BB.normalize box

                        newBoxies =
                            BB.insert id newBox model.boxies

                        newModel =
                            { model
                                | boxies = newBoxies
                                , hold = HoldNothing
                            }
                    in
                    ( newModel
                    , clipImageCommand newModel
                    )

        KeyDowned key ->
            case key of
                KeyDelete ->
                    let
                        newBoxies =
                            case model.select of
                                SelectNothing ->
                                    model.boxies

                                SelectBBox id ->
                                    BB.remove id model.boxies

                        newModel =
                            { model | boxies = newBoxies }
                    in
                    ( newModel
                    , clipImageCommand newModel
                    )

                KeyArrowLeft ->
                    let
                        newModel =
                            moveSelectBoxIfExists model -1 0
                    in
                    ( newModel
                    , clipImageCommand newModel
                    )

                KeyArrowUp ->
                    let
                        newModel =
                            moveSelectBoxIfExists model 0 -1
                    in
                    ( newModel
                    , clipImageCommand newModel
                    )

                KeyArrowRight ->
                    let
                        newModel =
                            moveSelectBoxIfExists model 1 0
                    in
                    ( newModel
                    , clipImageCommand newModel
                    )

                KeyArrowDown ->
                    let
                        newModel =
                            moveSelectBoxIfExists model 0 1
                    in
                    ( newModel
                    , clipImageCommand newModel
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        AddBox ->
            let
                ( w, h ) =
                    ( 100, 100 )

                ( svgWidth, svgHeight ) =
                    svgSizeWith
                        model.imgWidth
                        model.imgHeight

                newBox =
                    BB.BBox ((svgWidth - w) / 2) ((svgHeight - h) / 2) w h BB.None

                newBoxies =
                    BB.add newBox model.boxies

                newModel =
                    { model | boxies = newBoxies }
            in
            ( newModel
            , clipImageCommand newModel
            )

        CopyBox ->
            case model.select of
                SelectNothing ->
                    ( model, Cmd.none )

                SelectBBox id ->
                    case BB.get id model.boxies of
                        Nothing ->
                            ( model, Cmd.none )

                        Just box ->
                            let
                                newBoxies =
                                    BB.add box model.boxies

                                newModel =
                                    { model | boxies = newBoxies }
                            in
                            ( newModel
                            , clipImageCommand newModel
                            )

        ImageRequested ->
            ( model
            , Select.file [ "image/png", "image/jpeg", "image/gif", "image/bmp" ] ImageSelected
            )

        ImageSelected file ->
            ( model
            , Task.perform ImageUrlReceived (File.toUrl file)
            )

        ImageUrlReceived url ->
            ( { model
                | imgSrc = url
                , boxies = BB.empty
              }
            , askImageSize url
            )

        ImageSizeReceived w h ->
            let
                newModel =
                    { model
                        | imgWidth = w
                        , imgHeight = h
                    }
            in
            ( newModel
            , clipImageCommand newModel
            )

        ClippedImagesReceived list ->
            ( { model
                | clippedImages =
                    List.indexedMap
                        (\i x -> { url = x, name = String.fromInt i })
                        list
              }
            , Cmd.none
            )

        DownloadTar ->
            ( model
            , downloadBase64Tar model.clippedImages
            )

        ClippedImageNameChanged i name ->
            ( { model
                | clippedImages =
                    List.indexedMap
                        (\j e ->
                            if i == j then
                                { e | name = name }

                            else
                                e
                        )
                        model.clippedImages
              }
            , Cmd.none
            )

        FailedToLoadImage ->
            ( { model
                | imgSrc = ""
              }
            , Cmd.none
            )


updateSelectBoxIfExists : Model -> (BB.BBox -> BB.BBox) -> Model
updateSelectBoxIfExists ({ select, boxies } as model) f =
    case select of
        SelectNothing ->
            model

        SelectBBox id ->
            { model | boxies = BB.update id f boxies }


moveSelectBoxIfExists : Model -> Float -> Float -> Model
moveSelectBoxIfExists model dx dy =
    updateSelectBoxIfExists model
        (\c -> { c | x = c.x + dx, y = c.y + dy })


downloadBase64Tar : List ClippedImage -> Cmd Msg
downloadBase64Tar list =
    Download.bytes "images.tar"
        "application/x-tar"
        (base64sToTar list)


base64sToTar : List ClippedImage -> Bytes.Bytes
base64sToTar list =
    Tar.createArchive <|
        List.foldr
            (\{ name, url } acc ->
                case Base64.toBytes <| String.dropLeft 22 url of
                    Nothing ->
                        acc

                    Just byte ->
                        let
                            entry =
                                ( metadataWithPngName <| name ++ ".png"
                                , Tar.BinaryData byte
                                )
                        in
                        entry :: acc
            )
            []
            list


metadataWithPngName name =
    { defaultMetadata
        | filename = name
        , fileNamePrefix = "images"
    }


clipImageCommand : Model -> Cmd Msg
clipImageCommand model =
    let
        boxies =
            BB.scaleAll
                (svgSizeRatio
                    model.imgWidth
                    model.imgHeight
                )
                model.boxies
    in
    askClippedImages <|
        JE.object
            [ ( "src"
              , JE.string model.imgSrc
              )
            , ( "boxies"
              , JE.list
                    (\e ->
                        let
                            ( id, b ) =
                                e
                        in
                        JE.object
                            [ ( "id", JE.int id )
                            , ( "x", JE.float b.x )
                            , ( "y", JE.float b.y )
                            , ( "width", JE.float b.width )
                            , ( "height", JE.float b.height )
                            ]
                    )
                    (BB.toList boxies)
              )
            ]


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ viewMain model
        , viewSide model
        ]


viewMain : Model -> Html Msg
viewMain model =
    let
        ( svgWidth, svgHeight ) =
            svgSizeWith
                model.imgWidth
                model.imgHeight
    in
    S.svg
        [ SA.class "main"
        , style "border" "1px solid #000"
        , style "width" (String.fromFloat svgWidth)
        , style "height" (String.fromFloat svgHeight)
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        , attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
        ]
        (List.concat
            [ [ S.image
                    [ SA.id "image_input"
                    , SA.xlinkHref model.imgSrc
                    , SA.x "0"
                    , SA.y "0"
                    , SA.width (String.fromFloat svgWidth)
                    , SA.height (String.fromFloat svgHeight)
                    ]
                    []
              ]
            , BB.toMappedList (viewBBox model) model.boxies
            ]
        )


viewSide : Model -> Html Msg
viewSide model =
    div [ class "side" ]
        [ button [ onClick ImageRequested ] [ text "Load image" ]
        , button [ onClick AddBox ] [ text "Add" ]
        , button [ onClick CopyBox ] [ text "Copy" ]
        , div
            [ class "clipped-images"
            ]
            (List.indexedMap viewClippedImage model.clippedImages)
        , button [ onClick DownloadTar ] [ text "Download" ]
        ]


viewClippedImage : Int -> ClippedImage -> Html Msg
viewClippedImage i { name, url } =
    div [ class "clipped-image" ]
        [ img [ src url ] []
        , p []
            [ input
                [ onInput (ClippedImageNameChanged i)
                , value name
                , placeholder (String.fromInt i)
                ]
                []
            , span [] [ text ".png" ]
            ]
        ]


type alias Point =
    { x : Float
    , y : Float
    }


type alias BBoxEdge =
    { id : Int
    , from : Point
    , to : Point
    , pos : BB.BBoxPosition
    }


edgesOf : BB.Id -> BB.BBox -> List BBoxEdge
edgesOf id { width, height } =
    let
        ( p00, p01 ) =
            ( { x = 0, y = -edgeW / 2 }
            , { x = width, y = -edgeW / 2 }
            )

        ( p10, p11 ) =
            ( { x = width + edgeW / 2, y = 0 }
            , { x = width + edgeW / 2, y = height }
            )

        ( p20, p21 ) =
            ( { x = width, y = height + edgeW / 2 }
            , { x = 0, y = height + edgeW / 2 }
            )

        ( p30, p31 ) =
            ( { x = -edgeW / 2, y = height }
            , { x = -edgeW / 2, y = 0 }
            )
    in
    [ BBoxEdge id p00 p01 BB.Above
    , BBoxEdge id p10 p11 BB.Right
    , BBoxEdge id p20 p21 BB.Bottom
    , BBoxEdge id p30 p31 BB.Left
    ]


type alias BBoxCorner =
    { id : Int
    , point : Point
    , len : Float
    , pos : BB.BBoxPosition
    }


cornersOf : BB.Id -> BB.BBox -> List BBoxCorner
cornersOf id { width, height } =
    let
        p0 =
            { x = -edgeW, y = -edgeW }

        p1 =
            { x = width, y = -edgeW }

        p2 =
            { x = width, y = height }

        p3 =
            { x = -edgeW, y = height }
    in
    [ BBoxCorner id p0 edgeW BB.AboveLeft
    , BBoxCorner id p1 edgeW BB.AboveRight
    , BBoxCorner id p2 edgeW BB.BottomRight
    , BBoxCorner id p3 edgeW BB.BottomLeft
    ]


viewBBox : Model -> BB.Id -> BB.BBox -> Svg Msg
viewBBox model id box =
    S.g
        [ SA.transform <| translate box.x box.y
        ]
        [ viewBBoxLabel id
        , viewBBoxArea id box.width box.height
        , S.g [ SA.class "bbox-edges" ]
            (List.map (viewBBoxEdge model) (edgesOf id box))
        , S.g [ SA.class "bbox-corners" ]
            (List.map (viewBBoxCorner model) (cornersOf id box))
        ]


viewBBoxLabel : BB.Id -> Svg Msg
viewBBoxLabel id =
    S.g
        [ SA.transform (translate -10 -10)
        ]
        [ S.text_
            [ SA.style "user-select: none"
            , SA.dominantBaseline "central"
            , SA.textAnchor "middle"
            ]
            [ S.text (String.fromInt id) ]
        ]


viewBBoxArea : BB.Id -> Float -> Float -> Svg Msg
viewBBoxArea id width height =
    S.path
        [ SA.d <|
            String.join " "
                [ "M" ++ String.fromFloat 0 ++ "," ++ String.fromFloat 0
                , "L" ++ String.fromFloat width ++ "," ++ String.fromFloat 0
                , "L" ++ String.fromFloat width ++ "," ++ String.fromFloat height
                , "L" ++ String.fromFloat 0 ++ "," ++ String.fromFloat height
                , "Z"
                ]
        , SA.fill "#000"
        , SA.opacity "0.2"
        , SE.onMouseDown (Hold id BB.Center)
        , styleCursor BB.Center
        ]
        []


bboxColor : SelectState -> BB.Id -> String
bboxColor state id =
    case state of
        SelectNothing ->
            "#333"

        SelectBBox sid ->
            if sid == id then
                "#f80"

            else
                "#333"


viewBBoxCorner : Model -> BBoxCorner -> Svg Msg
viewBBoxCorner model { point, id, len, pos } =
    S.rect
        [ SA.x (String.fromFloat point.x)
        , SA.y (String.fromFloat point.y)
        , SA.fill <| bboxColor model.select id
        , SA.width (String.fromFloat len)
        , SA.height (String.fromFloat len)
        , SE.onMouseDown (Hold id pos)
        , styleCursor pos
        ]
        []


viewBBoxEdge : Model -> BBoxEdge -> Svg Msg
viewBBoxEdge model { from, to, id, pos } =
    S.path
        [ SA.d <|
            String.join " "
                [ "M" ++ String.fromFloat from.x ++ "," ++ String.fromFloat from.y
                , "L" ++ String.fromFloat to.x ++ "," ++ String.fromFloat to.y
                ]
        , SA.stroke <| bboxColor model.select id
        , SA.strokeWidth (String.fromFloat edgeW)
        , SE.onMouseDown <| Hold id pos
        , styleCursor pos
        ]
        []


styleCursor : BB.BBoxPosition -> S.Attribute Msg
styleCursor pos =
    case pos of
        BB.Above ->
            SA.style "cursor: ns-resize;"

        BB.Left ->
            SA.style "cursor: ew-resize;"

        BB.Bottom ->
            SA.style "cursor: ns-resize;"

        BB.Right ->
            SA.style "cursor: ew-resize;"

        BB.AboveLeft ->
            SA.style "cursor: nwse-resize;"

        BB.AboveRight ->
            SA.style "cursor: nesw-resize;"

        BB.BottomRight ->
            SA.style "cursor: nwse-resize;"

        BB.BottomLeft ->
            SA.style "cursor: nesw-resize;"

        BB.Center ->
            SA.style "cursor: move;"

        _ ->
            SA.style "cursor: default;"


translate : Float -> Float -> String
translate x y =
    "translate("
        ++ String.fromFloat x
        ++ ","
        ++ String.fromFloat y
        ++ ")"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ holdSub model
        , onKeyDownSub model
        , receiveImageSizeSub model
        , receiveClippedImagesSub model
        , failedToLoadImage (\_ -> FailedToLoadImage)
        ]


receiveClippedImagesSub : Model -> Sub Msg
receiveClippedImagesSub model =
    receiveClippedImages <|
        \v ->
            Result.withDefault (ClippedImagesReceived [])
                (JD.decodeValue
                    (JD.map ClippedImagesReceived
                        (JD.list JD.string)
                    )
                    v
                )


receiveImageSizeSub : Model -> Sub Msg
receiveImageSizeSub model =
    receiveImageSize <|
        \v ->
            Result.withDefault (ImageSizeReceived 0 0)
                (JD.decodeValue
                    (JD.map2 ImageSizeReceived
                        (JD.field "width" JD.float)
                        (JD.field "height" JD.float)
                    )
                    v
                )


holdSub : Model -> Sub Msg
holdSub model =
    let
        mousemove =
            BE.onMouseMove <|
                JD.map2 MouseMove
                    (JD.field "offsetX" JD.float)
                    (JD.field "offsetY" JD.float)
    in
    case model.hold of
        HoldNothing ->
            mousemove

        HoldBBox id pos ->
            Sub.batch
                [ mousemove
                , BE.onMouseMove <| JD.succeed <| Holding id pos
                , BE.onMouseUp <| JD.succeed <| MouseUp id
                ]


onKeyDownSub : Model -> Sub Msg
onKeyDownSub model =
    preventDefaultOnKeyDown <|
        \receivedKey ->
            Result.withDefault (KeyDowned (KeyOthers "none")) <|
                JD.decodeValue
                    (JD.map toKey (JD.field "key" JD.string))
                    receivedKey


toKey : String -> Msg
toKey key =
    case key of
        "Delete" ->
            KeyDowned KeyDelete

        "ArrowLeft" ->
            KeyDowned KeyArrowLeft

        "ArrowUp" ->
            KeyDowned KeyArrowUp

        "ArrowRight" ->
            KeyDowned KeyArrowRight

        "ArrowDown" ->
            KeyDowned KeyArrowDown

        c ->
            KeyDowned (KeyOthers c)
