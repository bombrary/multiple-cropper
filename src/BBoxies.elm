module BBoxies exposing (..)

import BBox exposing (..)
import Base64
import Bytes
import Dict as Dict exposing (Dict)
import Zip


type alias Id =
    Int


type alias BBoxies =
    { entities : Dict Id BBox
    , select : Maybe Id
    , hold : Maybe HoldInfo
    , nextId : Id
    }


type alias HoldInfo =
    { id : Id
    , anchor : Anchor
    }


empty : BBoxies
empty =
    { entities = Dict.empty
    , nextId = 0
    , select = Nothing
    , hold = Nothing
    }


toggleSelect : Maybe Id -> BBoxies -> BBoxies
toggleSelect id boxies =
    { boxies | select = id }


toggleHold : Maybe HoldInfo -> BBoxies -> BBoxies
toggleHold info boxies =
    { boxies | hold = info }


fromList : List BBoxOrigin -> BBoxies
fromList origins =
    let
        len =
            List.length origins
    in
    { entities =
        Dict.fromList <|
            List.indexedMap
                (\i origin ->
                    ( i
                    , { s = origin.s
                      , t = origin.t
                      , name = String.fromInt i ++ ".png"
                      , clippedImg = Nothing
                      }
                    )
                )
                origins
    , nextId = len
    , select = Nothing
    , hold = Nothing
    }


add : BBoxOrigin -> BBoxies -> BBoxies
add origin ({ entities, nextId } as boxies) =
    { boxies
        | entities =
            Dict.insert nextId
                { s = origin.s
                , t = origin.t
                , clippedImg = Nothing
                , name = String.fromInt nextId ++ ".png"
                }
                entities
        , nextId = nextId + 1
    }


get : Id -> BBoxies -> Maybe BBox
get i { entities } =
    Dict.get i entities


insert : Id -> BBox -> BBoxies -> BBoxies
insert i entity bboxies =
    { bboxies | entities = Dict.insert i entity bboxies.entities }


remove : Id -> BBoxies -> BBoxies
remove i bboxies =
    { bboxies | entities = Dict.remove i bboxies.entities }


map : (Id -> BBox -> BBox) -> BBoxies -> BBoxies
map f bboxies =
    { bboxies | entities = Dict.map f bboxies.entities }


update : Id -> (BBox -> BBox) -> BBoxies -> BBoxies
update i f bboxies =
    let
        g maybeE =
            case maybeE of
                Just e ->
                    Just (f e)

                Nothing ->
                    Nothing

        newEntities =
            Dict.update i g bboxies.entities
    in
    { bboxies | entities = newEntities }


toList : BBoxies -> List ( Id, BBox )
toList { entities } =
    Dict.toList entities


toListWith : (Bool -> Id -> BBox -> a) -> BBoxies -> List a
toListWith f { entities, select } =
    let
        isSelected id =
            case select of
                Nothing ->
                    False

                Just selectedId ->
                    id == selectedId
    in
    Dict.values <|
        Dict.map
            (\id bbox -> f (isSelected id) id bbox)
            entities


scaleAll : Float -> BBoxies -> BBoxies
scaleAll ratio bboxies =
    map (\i e -> scale ratio e) bboxies


updateHeldBox : Mouse -> BBoxies -> BBoxies
updateHeldBox mouse ({ hold } as bboxies) =
    case hold of
        Nothing ->
            bboxies

        Just { id, anchor } ->
            update id
                (normalize << transform mouse anchor)
                bboxies


getSelectedBox : BBoxies -> Maybe BBox
getSelectedBox { entities, select } =
    Maybe.andThen (\id -> Dict.get id entities)
        select


getHeldBox : BBoxies -> Maybe { id : Int, box : BBox }
getHeldBox { entities, hold } =
    Maybe.andThen
        (\{ id } ->
            Maybe.map
                (\box -> { id = id, box = box })
                (Dict.get id entities)
        )
        hold


toImages : BBoxies -> List { name : String, src : String }
toImages boxies =
    onlyJust <|
        toListWith
            (\_ id box ->
                case box.clippedImg of
                    Nothing ->
                        Nothing

                    Just src ->
                        Just
                            { name = validateName id box.name ++ ".png"
                            , src = src
                            }
            )
            boxies


onlyJust : List (Maybe a) -> List a
onlyJust xs =
    List.foldr
        (\maybeE acc ->
            case maybeE of
                Nothing ->
                    acc

                Just e ->
                    e :: acc
        )
        []
        xs


toZip : BBoxies -> Bytes.Bytes
toZip bboxies =
    let
        help { name, src } acc =
            case Base64.toBytes <| String.dropLeft 22 src of
                Nothing ->
                    acc

                Just byte ->
                    let
                        entry =
                            ( name, byte )
                    in
                    entry :: acc
    in
    Zip.fromList <|
        List.foldr help
            []
            (toImages bboxies)


validateName : Id -> String -> String
validateName id name =
    if name == "" then
        String.fromInt id

    else
        name


size : BBoxies -> Int
size { entities } =
    Dict.size entities
