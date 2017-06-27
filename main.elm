module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, custom, hardcoded)
import Table
import Time


--MODEL


type alias Model =
    { credit : Int
    , invalid : Bool
    , securities : List Security
    , securityTableState : Table.State
    , bankAcounts : List Acount
    , accountTableState : Table.State
    , totalBalance : Float
    , errorMessage : String
    }


type alias Acount =
    { bankName : String
    , balance : Float
    , currency : String
    , iban : String
    }


type alias Security =
    { name : String
    , price : Float
    , quantity : Int
    , totalValue : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { credit = 0
      , invalid = False
      , securities = []
      , securityTableState = Table.initialSort "name"
      , bankAcounts = []
      , accountTableState = Table.initialSort "balance"
      , errorMessage = ""
      , totalBalance = 0
      }
    , loadData
    )


loadData : Cmd Msg
loadData =
    Cmd.batch [ getAccounts, getSecurities ]


token : String
token =
    "Bearer ASHWLIkouP2O6_bgA2wWReRhletgWKHYjLqDaqb0LFfamim9RjexTo22ujRIP_cjLiRiSyQXyt2kM1eXU2XLFZQ0Hro15HikJQT_eNeT_9XQ"


getAccounts : Cmd Msg
getAccounts =
    Http.send Accounts <|
        Http.request
            { method = "GET"
            , headers = [ Http.header "Authorization" token ]
            , url = "https://api.figo.me/rest/accounts"
            , body = emptyBody
            , expect = expectJson decodeAcounts
            , timeout = Nothing
            , withCredentials = False
            }


getSecurities : Cmd Msg
getSecurities =
    Http.send Securities <|
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


decodeAcount : Decode.Decoder Acount
decodeAcount =
    decode Acount
        |> required "bank_name" Decode.string
        |> custom (Decode.at [ "balance", "balance" ] Decode.float)
        |> required "currency" Decode.string
        |> required "iban" Decode.string


decodeAcounts : Decode.Decoder (List Acount)
decodeAcounts =
    Decode.field "accounts" <|
        Decode.list <|
            decodeAcount



--Table


config : Table.Config Acount Msg
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


amountColumn : String -> (data -> Float) -> Table.Column data msg
amountColumn name toAmount =
    Table.customColumn
        { name = name
        , viewData = \data -> toString <| toAmount data
        , sorter = Table.decreasingOrIncreasingBy toAmount
        }


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



-- UPDATE


type Msg
    = Input String
    | Accounts (Result Http.Error (List Acount))
    | Securities (Result Http.Error (List Security))
    | Loading Time.Time
    | AccountTableState Table.State
    | SecurityTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            case String.toInt value of
                Ok credit ->
                    ( { model | invalid = False, credit = credit }, Cmd.none )

                Err _ ->
                    ( { model | invalid = True }, Cmd.none )

        Accounts (Ok accounts) ->
            ( { model | bankAcounts = accounts }, Cmd.none )

        Accounts (Err _) ->
            ( { model | errorMessage = "Could not load Banking data from figo" }, Cmd.none )

        Securities (Ok securities) ->
            let
                s =
                    securities
                        |> List.map (\o -> { o | totalValue = (toFloat o.quantity) * o.price })
            in
                ( { model | securities = s }, Cmd.none )

        Securities (Err _) ->
            ( { model | errorMessage = "Could not load Banking data from figo" }, Cmd.none )

        Loading _ ->
            ( model, loadData )

        AccountTableState newState ->
            ( { model | accountTableState = newState }, Cmd.none )

        SecurityTableState newState ->
            ( { model | securityTableState = newState }, Cmd.none )



--View


view : Model -> Html Msg
view model =
    let
        validation =
            if model.invalid then
                "This is not a number"
            else
                ""
    in
        div []
            [ h1 [] [ text "Was Kost Die Welt" ]
            , div [] [ text model.errorMessage ]
            , input [ onInput Input ] []
            , Table.view config model.accountTableState model.bankAcounts
            , h3 [] [ text <| toString <| totalBalance model.bankAcounts ]
            , Table.view securityConfig model.securityTableState model.securities
            , div [] [ text validation ]
            ]


viewAccounts : List Acount -> Html msg
viewAccounts accounts =
    List.map viewAccount accounts
        |> div []


viewAccount : Acount -> Html msg
viewAccount account =
    h3 [] [ text <| account.bankName ++ ": " ++ (toString account.balance) ++ " " ++ account.currency ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (10 * Time.second) Loading



-- UTIL


toCurrency : String -> Float -> String
toCurrency curr amount =
    curr
        |> (++) " "
        |> (++) (toString amount)


totalBalance : List Acount -> Float
totalBalance accounts =
    accounts
        |> List.map .balance
        |> List.foldr (+) 0
