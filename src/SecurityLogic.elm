module SecurityLogic exposing (..)

import Helper exposing (token, amountColumn)
import Html exposing (..)
import Http exposing (expectJson, emptyBody)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, custom, hardcoded)
import Table


type alias Model =
    { securities : List Security
    , securityTableState : Table.State
    , errorMessage : String
    }


type alias Security =
    { name : String
    , price : Float
    , quantity : Int
    , totalValue : Float
    }


init : Model
init =
    { securities = []
    , securityTableState = Table.initialSort "totalValue"
    , errorMessage = ""
    }


type Msg
    = GetSecurities (Result Http.Error (List Security))
    | SecurityTableState Table.State


update : Msg -> Model -> Model
update msg model =
    case msg of
        GetSecurities (Ok securities) ->
            let
                s =
                    securities
                        |> List.map (\o -> { o | totalValue = (toFloat o.quantity) * o.price })
            in
                { model | securities = s }

        GetSecurities (Err _) ->
            { model | errorMessage = "Could not load Banking data from figo" }

        SecurityTableState newState ->
            { model | securityTableState = newState }


view : Model -> Html Msg
view model =
    div []
        [ Table.view securityConfig model.securityTableState model.securities
        , h3 [] [ text model.errorMessage ]
        ]


securityConfig : Table.Config Security Msg
securityConfig =
    Table.config
        { toId = .name
        , toMsg = SecurityTableState
        , columns =
            [ Table.floatColumn "Amount" .totalValue
            , Table.stringColumn "Name" .name
            , Table.floatColumn "Price" .price
            , Table.intColumn "Quantity" .quantity
            ]
        }


request : Cmd Msg
request =
    Http.send GetSecurities getSecurities


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
