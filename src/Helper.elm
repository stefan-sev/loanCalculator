module Helper exposing (token, amountColumn)

import Table


token : String
token =
    "Bearer ASHWLIkouP2O6_bgA2wWReRhletgWKHYjLqDaqb0LFfamim9RjexTo22ujRIP_cjLiRiSyQXyt2kM1eXU2XLFZQ0Hro15HikJQT_eNeT_9XQ"


amountColumn : String -> (data -> Float) -> Table.Column data msg
amountColumn name toAmount =
    Table.customColumn
        { name = name
        , viewData = \data -> toString <| toAmount data
        , sorter = Table.decreasingOrIncreasingBy toAmount
        }
