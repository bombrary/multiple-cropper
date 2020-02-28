module BBox exposing (..)

import Vec as V exposing (Vec)


type Anchor
    = None
    | Center
    | Above
    | Right
    | Bottom
    | Left
    | AboveLeft
    | AboveRight
    | BottomRight
    | BottomLeft


type alias BBox =
    { s : Vec
    , t : Vec
    , holdPos : Anchor
    , clippedImg : Maybe String
    }


bbox : ( Float, Float ) -> ( Float, Float ) -> BBox
bbox s t =
    { s = V.fromTuple s
    , t = V.fromTuple t
    , holdPos = None
    , clippedImg = Nothing
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



{-
   transform : Position -> Anchor -> BBox -> BBox
   nextPosition : Position -> Anchor -> BBox -> Anchor
-}
