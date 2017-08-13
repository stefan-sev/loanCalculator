module Main exposing (..)

import BankAccount as Account
import Html exposing (..)
import Html.Events exposing (..)
import SecurityLogic as Security
import Time


--MODEL


type alias Model =
    { credit : Int
    , invalid : Bool
    , bankAccounts : Account.Model
    , securities : Security.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { credit = 0
      , invalid = False
      , securities = Security.init
      , bankAccounts = Account.init
      }
    , loadData
    )


loadData : Cmd Msg
loadData =
    Cmd.batch
        [ Cmd.map AccountMsg Account.request
        , Cmd.map SecurityMsg Security.request
        ]



-- UPDATE


type Msg
    = Input String
    | AccountMsg Account.Msg
    | SecurityMsg Security.Msg
    | Loading Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            case String.toInt value of
                Ok credit ->
                    ( { model | invalid = False, credit = credit }, Cmd.none )

                Err _ ->
                    ( { model | invalid = True }, Cmd.none )

        AccountMsg msg ->
            ( { model | bankAccounts = Account.update msg model.bankAccounts }, Cmd.none )

        SecurityMsg msg ->
            ( { model | securities = Security.update msg model.securities }, Cmd.none )

        Loading _ ->
            ( model, loadData )



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
            , input [ onInput Input ] []
            , Account.view model.bankAccounts
                |> Html.map AccountMsg
            , div [] [ text validation ]
            , Security.view model.securities
                |> Html.map SecurityMsg
            ]


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
