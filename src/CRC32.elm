module CRC32 exposing (..)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode


toUInt : Int -> Int
toUInt x =
    Bitwise.shiftRightZfBy 0 x


byteToUInt8 : Bytes -> Maybe (List Int)
byteToUInt8 bytes =
    let
        step : ( Int, List Int ) -> Decoder (Step ( Int, List Int ) (List Int))
        step ( cnt, xs ) =
            if cnt <= 0 then
                Decode.succeed (Done (List.reverse xs))

            else
                Decode.map (\x -> Loop ( cnt - 1, x :: xs )) Decode.unsignedInt8

        len =
            Bytes.width bytes
    in
    Decode.decode
        (Decode.loop ( len, [] ) step)
        bytes


encode : List Int -> Maybe Int
encode list =
    Maybe.map (Bitwise.xor 0xFFFFFFFF) <|
        List.foldl
            (\e maybeC ->
                case maybeC of
                    Nothing ->
                        Nothing

                    Just c ->
                        Maybe.map
                            (\arrElem ->
                                Bitwise.xor
                                    arrElem
                                    (Bitwise.shiftRightZfBy 8 c)
                            )
                            (Array.get
                                (Bitwise.and (Bitwise.xor c e) 0xFF)
                                table
                            )
            )
            (Just 0xFFFFFFFF)
            list


table : Array Int
table =
    Array.fromList <|
        List.map
            (\i ->
                List.foldr
                    (\j c ->
                        if Bitwise.and c 1 /= 0 then
                            Bitwise.xor
                                0xEDB88320
                                (Bitwise.shiftRightZfBy 1 c)

                        else
                            Bitwise.shiftRightZfBy 1 c
                    )
                    i
                    (List.range 0 7)
            )
            (List.range 0 255)


uIntToHex : Int -> String
uIntToHex x =
    let
        loop cnt rest acc =
            if cnt >= 8 then
                acc

            else
                loop (cnt + 1)
                    (Bitwise.shiftRightZfBy 4 rest)
                    (Bitwise.and 0x0F rest :: acc)
    in
    String.concat <|
        List.map
            (\n ->
                case n of
                    0 ->
                        "0"

                    1 ->
                        "1"

                    2 ->
                        "2"

                    3 ->
                        "3"

                    4 ->
                        "4"

                    5 ->
                        "5"

                    6 ->
                        "6"

                    7 ->
                        "7"

                    8 ->
                        "8"

                    9 ->
                        "9"

                    10 ->
                        "a"

                    11 ->
                        "b"

                    12 ->
                        "c"

                    13 ->
                        "d"

                    14 ->
                        "e"

                    15 ->
                        "f"

                    _ ->
                        "?"
            )
            (loop 0 x [])
