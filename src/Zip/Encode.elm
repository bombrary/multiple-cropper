module Zip.Encode exposing (..)

import Bytes as B exposing (Bytes, Endianness(..))
import Bytes.Encode as BE
import Zip.Config exposing (..)
import Zip.FileEntry exposing (..)


localFileHeader : LocalFileHeader -> BE.Encoder
localFileHeader l =
    BE.sequence
        [ BE.unsignedInt32 LE l.signature
        , BE.unsignedInt16 LE l.versionNeededToExtract
        , BE.unsignedInt16 LE l.generalPurposeBitFlag
        , BE.unsignedInt16 LE l.compressionMethod
        , BE.unsignedInt16 LE l.time
        , BE.unsignedInt16 LE l.date
        , BE.unsignedInt32 LE l.crc32
        , BE.unsignedInt32 LE l.compressedSize
        , BE.unsignedInt32 LE l.uncompressedSize
        , BE.unsignedInt16 LE l.fileNameLength
        , BE.unsignedInt16 LE l.extraFieldLength
        , BE.string l.fileName
        , BE.bytes l.extraField
        ]


centralDirectoryHeader : CentralDirectoryHeader -> BE.Encoder
centralDirectoryHeader c =
    BE.sequence
        [ BE.unsignedInt32 LE c.signature
        , BE.unsignedInt16 LE c.versionMadeBy
        , BE.unsignedInt16 LE c.versionNeededToExtract
        , BE.unsignedInt16 LE c.generalPurposeBitFlag
        , BE.unsignedInt16 LE c.compressionMethod
        , BE.unsignedInt16 LE c.lastModFileTime
        , BE.unsignedInt16 LE c.lastModFileDate
        , BE.unsignedInt32 LE c.crc32
        , BE.unsignedInt32 LE c.compressedSize
        , BE.unsignedInt32 LE c.uncompressedSize
        , BE.unsignedInt16 LE c.fileNameLength
        , BE.unsignedInt16 LE c.extraFieldLength
        , BE.unsignedInt16 LE c.fileCommentLength
        , BE.unsignedInt16 LE c.diskNumberStart
        , BE.unsignedInt16 LE c.internalFileAttributes
        , BE.unsignedInt32 LE c.externalFileAttributes
        , BE.unsignedInt32 LE c.relativeOffsetOfLocalHeader
        , BE.string c.fileName
        , BE.bytes c.extraField
        , BE.string c.fileComment
        ]


endOfCentralDirectory : EndOfCentralDirectory -> BE.Encoder
endOfCentralDirectory e =
    BE.sequence
        [ BE.unsignedInt32 LE e.signature
        , BE.unsignedInt16 LE e.numberOfThisDisk
        , BE.unsignedInt16 LE e.numberOfTheDiskWithTheStartOfTheCentralDirectory
        , BE.unsignedInt16 LE e.totalNumberOfEntriesInTheCentralDirectoryOnThisDisk
        , BE.unsignedInt16 LE e.totalNumberOfEntriesInTheCentralDirectory
        , BE.unsignedInt32 LE e.sizeOfTheCentralDirectory
        , BE.unsignedInt32 LE e.offsetOfStartOfCentralDirectory
        , BE.unsignedInt16 LE e.zipFileCommentLength
        , BE.string e.zipFileComment
        ]


zip : FileEntries -> BE.Encoder
zip { files, centralDirectory, offset } =
    let
        count =
            List.length files

        centralDirectoryBytes =
            BE.encode <| BE.sequence <| List.map centralDirectoryHeader centralDirectory

        eocd =
            { defaultEndOfCentralDirectory
                | totalNumberOfEntriesInTheCentralDirectoryOnThisDisk = count
                , totalNumberOfEntriesInTheCentralDirectory = count
                , sizeOfTheCentralDirectory = B.width centralDirectoryBytes
                , offsetOfStartOfCentralDirectory = offset
            }
    in
    BE.sequence
        [ BE.sequence <| List.map file files
        , BE.bytes centralDirectoryBytes
        , endOfCentralDirectory eocd
        ]


file : File -> BE.Encoder
file { header, body } =
    BE.sequence
        [ localFileHeader header
        , BE.bytes body
        ]
