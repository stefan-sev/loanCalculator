module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import BankAccount exposing (Account, getAccounts)
import SecurityLogic exposing (Security, getSecurities)
import Table
import Time


--MODEL


type alias Model =
    { credit : Int
    , invalid : Bool
    , securities : List Security
    , securityTableState : Table.State
    , bankAcounts : List Account
    , accountTableState : Table.State
    , totalBalance : Float
    , errorMessage : String
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
    Cmd.batch
        [ Http.send GetAccounts getAccounts
        , Http.send GetSecurities getSecurities
        ]



--Table


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
    | GetAccounts (Result Http.Error (List Account))
    | GetSecurities (Result Http.Error (List Security))
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

        GetAccounts (Ok accounts) ->
            ( { model | bankAcounts = accounts }, Cmd.none )

        GetAccounts (Err _) ->
            ( { model | errorMessage = "Could not load Banking data from figo" }, Cmd.none )

        GetSecurities (Ok securities) ->
            let
                s =
                    securities
                        |> List.map (\o -> { o | totalValue = (toFloat o.quantity) * o.price })
            in
                ( { model | securities = s }, Cmd.none )

        GetSecurities (Err _) ->
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


viewAccounts : List Account -> Html msg
viewAccounts accounts =
    List.map viewAccount accounts
        |> div []


viewAccount : Account -> Html msg
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


totalBalance : List Account -> Float
totalBalance accounts =
    accounts
        |> List.map .balance
        |> List.foldr (+) 0
