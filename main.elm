module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, custom)


type alias Model =
    { credit : Int
    , invalid : Bool
    , savings : List Saving
    , bankAcounts : List Acount
    , totalBalance : Float
    , errorMessage : String
    }


type alias Acount =
    { bankName : String
    , balance : Float
    }


type alias Saving =
    { name : String
    , value : Float
    , amount : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { credit = 0, invalid = False, savings = [], bankAcounts = [], errorMessage = "", totalBalance = 0 }, getAccounts )


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


decodeAcount : Decode.Decoder Acount
decodeAcount =
    decode Acount
        |> required "bank_name" Decode.string
        |> custom (Decode.at [ "balance", "balance" ] Decode.float)


decodeAcounts : Decode.Decoder (List Acount)
decodeAcounts =
    Decode.field "accounts" <|
        Decode.list <|
            decodeAcount


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
    Sub.none


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
            , div [] [ viewAccounts model.bankAcounts ]
            , h3 [] [ text <| toString <| totalBalance model.bankAcounts ]
            , input [ onInput Input ] []
            , div [] [ text validation ]
            ]


totalBalance : List Acount -> Float
totalBalance accounts =
    accounts
        |> List.map .balance
        |> List.foldr (+) 0


viewAccounts : List Acount -> Html msg
viewAccounts accounts =
    div [] <| List.map viewAccount accounts


viewAccount : Acount -> Html msg
viewAccount account =
    h3 [] [ text <| account.bankName ++ ": " ++ (toString account.balance) ]


type Msg
    = Input String
    | Accounts (Result Http.Error (List Acount))


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
