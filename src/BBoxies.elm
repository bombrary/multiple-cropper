module BBoxies exposing (..)

import BBox exposing (..)
import Dict as Dict exposing (Dict)


type alias Id =
    Int


type alias BBoxies =
    { entities : Dict Id BBox
    , select : Maybe Id
    , hold : Maybe Id
    , nextId : Id
    }


empty : BBoxies
empty =
    { entities = Dict.empty
    , nextId = 0
    , select = Nothing
    , hold = Nothing
    }


toggleSelect : Id -> BBoxies -> BBoxies
toggleSelect id boxies =
    case boxies.select of
        Nothing ->
            { boxies | select = Just id }

        Just _ ->
            { boxies | select = Nothing }


toggleHold : Id -> BBoxies -> BBoxies
toggleHold id boxies =
    case boxies.hold of
        Nothing ->
            { boxies | hold = Just id }

        Just _ ->
            { boxies | hold = Nothing }


fromList : List BBox -> BBoxies
fromList xs =
    let
        len =
            List.length xs
    in
    { entities =
        Dict.fromList <|
            List.map2 Tuple.pair (List.range 0 (len - 1)) xs
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


toMappedList : (Id -> BBox -> a) -> BBoxies -> List a
toMappedList f { entities } =
    Dict.values <| Dict.map f entities


sample : BBoxies
sample =
    fromList [ bbox0, bbox1, bbox2 ]


scaleAll : ( Float, Float ) -> BBoxies -> BBoxies
scaleAll ratio bboxies =
    map (\i e -> scale ratio e) bboxies
