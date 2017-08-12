module BankAccount exposing (..)

import Http exposing (expectJson, emptyBody)
import Json.Decode.Pipeline exposing (decode, required, custom, hardcoded)
import Json.Decode as Decode
import Helper exposing (token)


{--token : String
token =
    "Bearer ASHWLIkouP2O6_bgA2wWReRhletgWKHYjLqDaqb0LFfamim9RjexTo22ujRIP_cjLiRiSyQXyt2kM1eXU2XLFZQ0Hro15HikJQT_eNeT_9XQ"
--}


type alias Account =
    { bankName : String
    , balance : Float
    , currency : String
    , iban : String
    }


getAccounts : Http.Request (List Account)
getAccounts =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "https://api.figo.me/rest/accounts"
        , body = emptyBody
        , expect = expectJson decodeAcounts
        , timeout = Nothing
        , withCredentials = False
        }


decodeAcount : Decode.Decoder Account
decodeAcount =
    decode Account
        |> required "bank_name" Decode.string
        |> custom (Decode.at [ "balance", "balance" ] Decode.float)
        |> required "currency" Decode.string
        |> required "iban" Decode.string


decodeAcounts : Decode.Decoder (List Account)
decodeAcounts =
    Decode.field "accounts" <|
        Decode.list <|
            decodeAcount
