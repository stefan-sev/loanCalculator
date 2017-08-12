module SecurityLogic exposing (..)

import Http exposing (expectJson, emptyBody)
import Json.Decode.Pipeline exposing (decode, required, custom, hardcoded)
import Json.Decode as Decode
import Helper exposing (token)


type alias Security =
    { name : String
    , price : Float
    , quantity : Int
    , totalValue : Float
    }


getSecurities : Http.Request (List Security)
getSecurities =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "https://api.figo.me/rest/securities"
        , body = emptyBody
        , expect = expectJson decodeSecurities
        , timeout = Nothing
        , withCredentials = False
        }


decodeSecurities : Decode.Decoder (List Security)
decodeSecurities =
    Decode.field "securities" <|
        Decode.list <|
            decodeSecurity


decodeSecurity : Decode.Decoder Security
decodeSecurity =
    decode Security
        |> required "name" Decode.string
        |> required "price" Decode.float
        |> required "quantity" Decode.int
        |> hardcoded 0
