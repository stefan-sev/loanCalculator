module BankAccount exposing (update, Msg(..), view, Model, request, init)

import Helper exposing (token, amountColumn)
import Html exposing (..)
import Http exposing (expectJson, emptyBody)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, custom, hardcoded)
import Table


type Msg
    = AccountTableState Table.State
    | GetAccounts (Result Http.Error (List Account))


type alias Model =
    { accounts : List Account
    , errorMessage : String
    , tableState : Table.State
    , balance : Float
    }


type alias Account =
    { bankName : String
    , balance : Float
    , currency : String
    , iban : String
    }


init : Model
init =
    { accounts = []
    , errorMessage = ""
    , tableState = Table.initialSort "balance"
    , balance = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        GetAccounts (Ok accounts) ->
            { model | accounts = accounts, balance = totalBalance accounts }

        GetAccounts (Err _) ->
            { model | errorMessage = "Could not load Banking data from figo" }

        AccountTableState state ->
            { model | tableState = state }


view : Model -> Html Msg
view model =
    div []
        [ Table.view config model.tableState model.accounts
        , h3 [] [ text <| toString <| totalBalance model.accounts ]
        ]


request : Cmd Msg
request =
    Http.send GetAccounts getAccounts


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


totalBalance : List Account -> Float
totalBalance accounts =
    accounts
        |> List.map .balance
        |> List.foldr (+) 0


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


config : Table.Config Account Msg
config =
    Table.config
        { toId = .iban
        , toMsg = AccountTableState
        , columns =
            [ amountColumn "Balance" .balance
            , Table.stringColumn "Name" .bankName
            , Table.stringColumn "Currency" .currency
            ]
        }
