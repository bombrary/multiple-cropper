module BBox exposing (..)


type BBoxPosition
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
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , hold : BBoxPosition
    }


type alias Position =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }


transform : Position -> BBoxPosition -> BBox -> BBox
transform { x, y, dx, dy } pos bbox =
    let
        ( sx, sy ) =
            ( bbox.x, bbox.y )

        ( ex, ey ) =
            ( sx + bbox.width, sy + bbox.height )

        new =
            case pos of
                Center ->
                    { sx = sx + dx
                    , sy = sy + dy
                    , ex = ex + dx
                    , ey = ey + dy
                    }

                Above ->
                    { sx = sx
                    , sy = y
                    , ex = ex
                    , ey = ey
                    }

                Right ->
                    { sx = sx
                    , sy = sy
                    , ex = x
                    , ey = ey
                    }

                Bottom ->
                    { sx = sx
                    , sy = sy
                    , ex = ex
                    , ey = y
                    }

                Left ->
                    { sx = x
                    , sy = sy
                    , ex = ex
                    , ey = ey
                    }

                AboveLeft ->
                    { sx = x
                    , sy = y
                    , ex = ex
                    , ey = ey
                    }

                AboveRight ->
                    { sx = sx
                    , sy = y
                    , ex = x
                    , ey = ey
                    }

                BottomRight ->
                    { sx = sx
                    , sy = sy
                    , ex = x
                    , ey = y
                    }

                BottomLeft ->
                    { sx = x
                    , sy = sy
                    , ex = ex
                    , ey = y
                    }

                _ ->
                    { sx = sx
                    , sy = sy
                    , ex = ex
                    , ey = ey
                    }
    in
    { bbox
        | x = new.sx
        , y = new.sy
        , width = new.ex - new.sx
        , height = new.ey - new.sy
    }


nextPosition : Position -> BBoxPosition -> BBox -> BBoxPosition
nextPosition { dx, dy } pos { x, y, height, width } =
    let
        ( sx, sy ) =
            ( x, y )

        ( ex, ey ) =
            ( sx + width, sy + height )
    in
    case pos of
        Above ->
            if sy > ey then
                Bottom

            else
                Above

        Right ->
            if sy < ey then
                Left

            else
                Right

        Bottom ->
            if sy < ey then
                Above

            else
                Bottom

        Left ->
            if sx > ex then
                Right

            else
                Left

        _ ->
            pos


normalize : BBox -> BBox
normalize { x, y, width, height, hold } =
    let
        list =
            List.sort
                [ ( x, y )
                , ( x + width, y )
                , ( x, y + height )
                , ( x + width, y + height )
                ]
    in
    case list of
        [ a, b, c, d ] ->
            let
                ( x0, y0 ) =
                    a

                ( x1, y1 ) =
                    d

                ( newW, newH ) =
                    ( x1 - x0, y1 - y0 )
            in
            BBox x0 y0 newW newH hold

        _ ->
            BBox x y width height hold


scale : ( Float, Float ) -> BBox -> BBox
scale ratio b =
    let
        ( rw, rh ) =
            ratio
    in
    { b
        | x = b.x * rw
        , y = b.y * rh
        , width = b.width * rw
        , height = b.height * rh
    }


bbox0 : BBox
bbox0 =
    BBox 100 50 50 50 None


bbox1 : BBox
bbox1 =
    BBox 20 10 12 46 None


bbox2 : BBox
bbox2 =
    BBox 200 10 12 46 None
