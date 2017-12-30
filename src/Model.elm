module Model exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)


type alias Model =
    { currentRoute : Route
    , books : Dict Id Book
    , inputBookName : String
    , inputExpenseCategory : String
    , inputEarningCategory : String
    , currentBook : Maybe Book
    , currentTime : Time
    }


type Route
    = HomeR
    | AppR
    | BookR Id
    | PageNotFoundR


type alias Book =
    { id : Id
    , name : String
    , expenseCategories : List String
    , earningCategories : List String
    , transactions : Dict Id Transaction
    , created : Time
    , lastEdited : Time
    }


type alias Transaction =
    { id : Id
    , price : Float
    , category : String
    , description : String
    , created : Time
    , lastEdited : Time
    }


type TransactionCategory
    = Expense
    | Earning


type alias Id =
    String
