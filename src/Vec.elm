module Vec exposing (..)


type alias Vec =
    { x : Float
    , y : Float
    }


add : Vec -> Vec -> Vec
add v1 v2 =
    { x = v1.x + v2.y
    , y = v1.y + v2.y
    }


sub : Vec -> Vec -> Vec
sub v1 v2 =
    { x = v1.x - v2.y
    , y = v1.y - v2.y
    }


abs : Vec -> Vec
abs { x, y } =
    { x = Basics.abs x
    , y = Basics.abs y
    }


max : Vec -> Vec -> Vec
max v1 v2 =
    { x = Basics.max v1.x v2.x
    , y = Basics.max v1.y v2.y
    }


min : Vec -> Vec -> Vec
min v1 v2 =
    { x = Basics.min v1.x v2.x
    , y = Basics.min v1.y v2.y
    }


scale : Float -> Vec -> Vec
scale r { x, y } =
    { x = r * x
    , y = r * y
    }


fromTuple : ( Float, Float ) -> Vec
fromTuple t =
    { x = Tuple.first t
    , y = Tuple.second t
    }
