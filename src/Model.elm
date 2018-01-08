module Model exposing (..)

import Date exposing (Month)
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
    , confirmDeleteBook : Bool
    , selectedBookToDelete : Maybe Book
    , transactionsToDisplay : List Transaction
    , selectedMonth : Month
    , selectedYear : Int
    , currentDisplay : TransactionsDisplay
    , query : String
    , confirmDeleteTransaction : Bool
    , selectedTransactionToDelete : Maybe Transaction
    , selectedFilterType : String
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


type TransactionsDisplay
    = All
    | ByYear Int
    | ByMonth Month Int



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
