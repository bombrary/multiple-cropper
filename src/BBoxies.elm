module BBoxies exposing (..)

import BBox exposing (..)
import Dict as Dict exposing (Dict)


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


add : BBox -> BBoxies -> BBoxies
add entity ({ entities, nextId } as boxies) =
    { boxies
        | entities = Dict.insert nextId entity entities
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
                (transform mouse anchor)
                bboxies
