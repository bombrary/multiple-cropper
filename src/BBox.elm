module BBox exposing (..)

import Vec as V exposing (Vec)


type alias Mouse =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }


type Anchor
    = Inner
    | Above
    | Right
    | Below
    | Left
    | AboveLeft
    | AboveRight
    | BelowRight
    | BelowLeft


type alias BBox =
    { s : Vec
    , t : Vec
    , name : String
    , clippedImg : Maybe String
    }


type alias BBoxOrigin =
    { s : Vec
    , t : Vec
    }


bboxOrigin : ( Float, Float ) -> ( Float, Float ) -> BBoxOrigin
bboxOrigin s t =
    { s = V.fromTuple s
    , t = V.fromTuple t
    }


type alias Position =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }


width : BBox -> Float
width { s, t } =
    (V.abs <| V.sub t s).x


height : BBox -> Float
height { s, t } =
    (V.abs <| V.sub t s).y


normalize : BBox -> BBox
normalize ({ s, t } as box) =
    { box
        | s = V.min s t
        , t = V.max s t
    }


scale : Float -> BBox -> BBox
scale r ({ s, t } as box) =
    { box
        | s = V.scale r s
        , t = V.scale r t
    }


transform : Mouse -> Anchor -> BBox -> BBox
transform { dx, dy } anchor box =
    case anchor of
        Inner ->
            { box
                | s = V.add box.s (Vec dx dy)
                , t = V.add box.t (Vec dx dy)
            }

        Above ->
            { box
                | s = V.add box.s (Vec 0 dy)
            }

        Right ->
            { box
                | t = V.add box.t (Vec dx 0)
            }

        Below ->
            { box
                | t = V.add box.t (Vec 0 dy)
            }

        Left ->
            { box
                | s = V.add box.s (Vec dx 0)
            }

        AboveLeft ->
            { box
                | s = V.add box.s (Vec dx dy)
            }

        AboveRight ->
            { box
                | s = V.add box.s (Vec 0 dy)
                , t = V.add box.t (Vec dx 0)
            }

        BelowRight ->
            { box
                | t = V.add box.t (Vec dx dy)
            }

        BelowLeft ->
            { box
                | s = V.add box.s (Vec dx 0)
                , t = V.add box.t (Vec 0 dy)
            }
