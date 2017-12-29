module Model exposing (..)

import Dict exposing (Dict)


type alias Model =
    { currentRoute : Route
    , books : Dict Id Book
    , inputBookName : String
    , inputExpenseCategory : String
    , inputEarningCategory : String
    , currentBook : Maybe Book
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
    }


type alias Transaction =
    { id : Id
    , price : Float
    , category : String
    , description : String
    }


type TransactionCategory
    = Expense
    | Earning


type alias Id =
    String
