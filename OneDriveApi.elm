module OneDriveApi where

import Effects exposing (Never)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (Task)


type alias Item =
  { id : String
  , name : String
  , folder : Maybe FolderFacet
  , file : Maybe FileFacet
  }


type alias FolderFacet =
  { childCount : Int
  }


type alias FileFacet =
  { mimeType : String
  }


oneDriveGetChildren : String -> String -> Task Never (Result String (List Item))
oneDriveGetChildren accessToken path =
  Http.get ("value" := (Json.list decodeItem)) (getOneDriveFolderChildrenUrl accessToken path)
    |> Task.toResult
    |> Task.map (Result.formatError convertError)
         

decodeItem : Json.Decoder Item
decodeItem =
  Json.object4 Item
        ("id" := Json.string)
        ("name" := Json.string)
        (Json.maybe ("folder" := decodeFolderFacet))
        (Json.maybe ("file" := decodeFileFacet))


decodeFileFacet : Json.Decoder FileFacet
decodeFileFacet =
  Json.object1 FileFacet ("mimeType" := Json.string)


decodeFolderFacet : Json.Decoder FolderFacet
decodeFolderFacet =
  Json.object1 FolderFacet ("childCount" := Json.int)


getOneDriveFolderChildrenUrl : String -> String -> String
getOneDriveFolderChildrenUrl accessToken path =
  Http.url ("https://api.onedrive.com/v1.0/drive/root:" ++ path ++ ":/children") [("access_token", accessToken)]


convertError : Http.Error -> String
convertError err =
  case err of
    Http.BadResponse _ msg -> msg
    _ -> "Unknown error"


