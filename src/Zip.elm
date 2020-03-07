module Zip exposing (..)

import Bytes as B exposing (Bytes, Endianness(..))
import Bytes.Encode as BE exposing (Encoder)
import CRC32 as C
import Zip.Config as ZC
import Zip.Encode as ZE
import Zip.FileEntry as ZFE


fromList : List ( String, Bytes ) -> Bytes
fromList list =
    let
        files =
            onlyJust <| List.map (\( name, body ) -> ZC.makeFile name body) list
    in
    BE.encode <|
        ZE.zip <|
            ZFE.fromFiles files


sampleZip =
    fromList
        [ ( "foo.txt", BE.encode <| BE.string "Hello, World" )
        , ( "bar.txt", BE.encode <| BE.string "Good bye" )
        ]


onlyJust : List (Maybe a) -> List a
onlyJust xs =
    List.foldr
        (\x acc ->
            case x of
                Nothing ->
                    acc

                Just y ->
                    y :: acc
        )
        []
        xs
