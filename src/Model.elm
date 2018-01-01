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
    , selectedCategory : String
    , selectedCategoryType : CategoryType
    , inputPrice : String
    , inputDescription : String
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


type CategoryType
    = Expense
    | Earning


type alias Id =
    String


type alias SummaryByCategory =
    { includedInChart : Bool
    , category : String
    , totalPrice : Float
    }



---- MODEL INIT ----


defaultExpenseCategories : List String
defaultExpenseCategories =
    [ "Uncategorized", "Food", "Rent", "Transpo", "Leisure", "Misc", "Subscription", "Medical", "Unexpected" ]


defaultEarningCategories : List String
defaultEarningCategories =
    [ "Uncategorized", "Salary", "Bonus", "Gift", "Reimbursement", "Sideline", "Unexpected" ]


dummyBook : Id -> Book
dummyBook id =
    Book ("dummy" ++ id) ("dummy" ++ id) [] [] Dict.empty 0.0 0.0


initBooks : Dict String Book
initBooks =
    Dict.empty