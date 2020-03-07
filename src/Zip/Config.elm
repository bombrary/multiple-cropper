module Zip.Config exposing (..)

import Bytes as B exposing (Bytes)
import Bytes.Encode as BE
import CRC32 as C


type alias Byte4 =
    Int


type alias Byte2 =
    Int


type alias LocalFileHeader =
    { signature : Byte4
    , versionNeededToExtract : Byte4
    , generalPurposeBitFlag : Byte2
    , compressionMethod : Byte2
    , time : Byte2
    , date : Byte2
    , crc32 : Byte4
    , compressedSize : Byte4
    , uncompressedSize : Byte4
    , fileNameLength : Byte2
    , extraFieldLength : Byte2
    , fileName : String
    , extraField : Bytes
    }


defaultLocalFileHeader : LocalFileHeader
defaultLocalFileHeader =
    { signature = 0x04034B50
    , versionNeededToExtract = 0x0A
    , generalPurposeBitFlag = 0x00
    , compressionMethod = 0x00
    , time = 0x00
    , date = 0x00
    , crc32 = 0x00
    , compressedSize = 0x00
    , uncompressedSize = 0x00
    , fileNameLength = 0x00
    , extraFieldLength = 0x00
    , fileName = ""
    , extraField = BE.encode (BE.string "")
    }


type alias CentralDirectoryHeader =
    { signature : Byte4
    , versionMadeBy : Byte2
    , versionNeededToExtract : Byte2
    , generalPurposeBitFlag : Byte2
    , compressionMethod : Byte2
    , lastModFileTime : Byte2
    , lastModFileDate : Byte2
    , crc32 : Byte4
    , compressedSize : Byte4
    , uncompressedSize : Byte4
    , fileNameLength : Byte2
    , extraFieldLength : Byte2
    , fileCommentLength : Byte2
    , diskNumberStart : Byte2
    , internalFileAttributes : Byte2
    , externalFileAttributes : Byte4
    , relativeOffsetOfLocalHeader : Byte4
    , fileName : String
    , extraField : Bytes
    , fileComment : String
    }


defaultCentralDirectoryHeader : CentralDirectoryHeader
defaultCentralDirectoryHeader =
    { signature = 0x02014B50
    , versionMadeBy = 0x030A
    , versionNeededToExtract = 0x03
    , generalPurposeBitFlag = 0x00
    , compressionMethod = 0x00
    , lastModFileTime = 0x00
    , lastModFileDate = 0x00
    , crc32 = 0x00
    , compressedSize = 0x00
    , uncompressedSize = 0x00
    , fileNameLength = 0x00
    , extraFieldLength = 0x00
    , fileCommentLength = 0x00
    , diskNumberStart = 0x00
    , internalFileAttributes = 0x00
    , externalFileAttributes = 0x00
    , relativeOffsetOfLocalHeader = 0x00
    , fileName = ""
    , extraField = BE.encode (BE.string "")
    , fileComment = ""
    }


type alias EndOfCentralDirectory =
    { signature : Byte4
    , numberOfThisDisk : Byte2
    , numberOfTheDiskWithTheStartOfTheCentralDirectory : Byte2
    , totalNumberOfEntriesInTheCentralDirectoryOnThisDisk : Byte2
    , totalNumberOfEntriesInTheCentralDirectory : Byte2
    , sizeOfTheCentralDirectory : Byte4
    , offsetOfStartOfCentralDirectory : Byte4
    , zipFileCommentLength : Byte2
    , zipFileComment : String
    }


defaultEndOfCentralDirectory : EndOfCentralDirectory
defaultEndOfCentralDirectory =
    { signature = 0x06054B50
    , numberOfThisDisk = 0x00
    , numberOfTheDiskWithTheStartOfTheCentralDirectory = 0x00
    , totalNumberOfEntriesInTheCentralDirectoryOnThisDisk = 0x00
    , totalNumberOfEntriesInTheCentralDirectory = 0x00
    , sizeOfTheCentralDirectory = 0x00
    , offsetOfStartOfCentralDirectory = 0x00
    , zipFileCommentLength = 0x00
    , zipFileComment = ""
    }


type alias File =
    { header : LocalFileHeader
    , body : Bytes
    }


makeFile : String -> Bytes -> Maybe File
makeFile name body =
    Maybe.map
        (\code ->
            { header =
                { defaultLocalFileHeader
                    | fileNameLength = String.length name
                    , fileName = name
                    , compressedSize = B.width body
                    , uncompressedSize = B.width body
                    , crc32 = code
                }
            , body = body
            }
        )
        (Maybe.andThen C.encode <| C.byteToUInt8 <| body)


centralDirectoryHeaderFromFile : File -> CentralDirectoryHeader
centralDirectoryHeaderFromFile { header } =
    { defaultCentralDirectoryHeader
        | fileNameLength = header.fileNameLength
        , fileName = header.fileName
        , compressedSize = header.compressedSize
        , uncompressedSize = header.uncompressedSize
        , crc32 = header.crc32
    }
