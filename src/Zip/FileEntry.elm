module Zip.FileEntry exposing (..)

import Bytes as B
import Zip.Config exposing (..)


type alias FileEntries =
    { files : List File
    , centralDirectory : List CentralDirectoryHeader
    , offset : Int
    }


fromFiles : List File -> FileEntries
fromFiles files =
    let
        lackedCentralDirectory =
            List.map centralDirectoryHeaderFromFile files

        fileWidths =
            List.map fileWidth files

        fileOffsets =
            scanl (+) 0 fileWidths

        centralDirectory =
            List.map2
                (\offset cd -> { cd | relativeOffsetOfLocalHeader = offset })
                fileOffsets
                lackedCentralDirectory
    in
    { files = files
    , centralDirectory = centralDirectory
    , offset = List.sum fileWidths
    }


scanl : (a -> b -> b) -> b -> List a -> List b
scanl f b xs =
    let
        scan1 x list =
            case list of
                acc :: _ ->
                    f x acc :: list

                [] ->
                    []
    in
    List.reverse <| List.drop 1 (List.foldl scan1 [ b ] xs)


fileWidth : File -> Int
fileWidth { header, body } =
    30
        + String.length header.fileName
        + B.width header.extraField
        + B.width body
