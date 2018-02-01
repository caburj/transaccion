port module Main exposing (..)

-- elm-package install --yes elm-community/json-extra

import Chart as C exposing (..)
import Date exposing (Date, Month(..))
import Date.Extra as Date
import Dict exposing (Dict)
import Dom
import Hashids exposing (hashidsMinimum)
import Helper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Json_ exposing (decodeBooks, encodeBook, encodeBooks)
import List.Extra exposing (unique)
import Model exposing (..)
import Navigation as Nav
import Round
import Task
import Time exposing (Time)
import UrlParser as Url exposing ((</>))


---- PROGRAM ----


main : Program (Maybe String) Model Msg
main =
    Nav.programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- PORTS ----


port deleteBook : Id -> Cmd msg


port saveBook : String -> Cmd msg


init : Maybe String -> Nav.Location -> ( Model, Cmd Msg )
init maybeBooks loc =
    case maybeBooks of
        Nothing ->
            Model HomeR initBooks "" "" "" Nothing 0 "" Expense "" "" False Nothing [] Jan 0 All "" False Nothing "Any" ! [ getTimeNow ]

        Just strBook ->
            let
                resultBooks =
                    decodeString decodeBooks strBook

                books =
                    case resultBooks of
                        Ok books_ ->
                            books_

                        Err _ ->
                            initBooks
            in
            Model HomeR books "" "" "" Nothing 0 "" Expense "" "" False Nothing [] Jan 0 All "" False Nothing "Any" ! [ getTimeNow ]


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map HomeR Url.top
        , Url.map AppR (Url.s "books")
        , Url.map BookR (Url.s "books" </> Url.string)
        ]



---- UPDATE ----


type Msg
    = UrlChange Nav.Location
    | TimeNow Time
    | NewUrl String
    | SelectBook Id
    | InputBookName String
    | AddBook String
    | DeleteBook Id
    | ConfirmDeleteBook (Maybe Book)
    | CancelDeleteBook
    | DeleteExpenseCategory Id String
    | DeleteEarningCategory Id String
    | InputCategory CategoryType String
    | AddCategory CategoryType
    | SelectTransactionCategory CategoryType
    | SelectCategory String
    | InputPrice String
    | InputDescription String
    | ChangeCategoryType
    | ChangeCategory String
    | AddTransaction
    | ConfirmDeleteTransaction (Maybe Transaction)
    | CancelDeleteTransaction
    | DeleteTransaction Id
    | CancelTransactionInput
    | CalculateTransactionsToDisplay TransactionsDisplay
    | ChangeTransactionsDisplay TransactionsDisplay
    | SelectMonth String
    | SelectYear String
    | InputQuery String
    | ChangeFilterType String
    | FocusOn String
    | FocusResult (Result Dom.Error ())
    | NoOp
    | ClearBookInputs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeNow time ->
            { model | currentTime = time } ! []

        ClearBookInputs ->
            { model | inputExpenseCategory = "", inputEarningCategory = "" } ! []

        NewUrl url ->
            model ! [ Nav.newUrl url, getTimeNow ]

        UrlChange loc ->
            let
                cRoute =
                    case Url.parsePath route loc of
                        Nothing ->
                            PageNotFoundR

                        Just r ->
                            r
            in
            update ClearBookInputs { model | currentRoute = cRoute }

        SelectBook bookId ->
            let
                ( newModel, cmds ) =
                    model ! [ getTimeNow ]

                cBook =
                    Maybe.withDefault (dummyBook "dummy") (Dict.get bookId newModel.books)

                category =
                    case newModel.selectedCategoryType of
                        Expense ->
                            Maybe.withDefault "Uncategorized" (List.head cBook.expenseCategories)

                        Earning ->
                            Maybe.withDefault "Uncategorized" (List.head cBook.earningCategories)

                year =
                    newModel.currentTime
                        |> Date.fromTime
                        |> Date.year

                month =
                    newModel.currentTime
                        |> Date.fromTime
                        |> Date.month
            in
            update (ChangeTransactionsDisplay (ByMonth month year))
                { newModel
                    | selectedCategory = category
                    , selectedMonth = month
                    , selectedYear = year
                    , currentBook = Just cBook
                    , currentDisplay = All
                }

        InputBookName bookName ->
            { model | inputBookName = bookName } ! []

        AddBook name ->
            let
                ( newModel, cmds ) =
                    model ! [ getTimeNow ]
            in
            case name of
                "" ->
                    newModel ! []

                _ ->
                    let
                        time =
                            newModel.currentTime

                        id =
                            time
                                |> floor
                                |> Hashids.encode salt

                        newBook =
                            Book id name defaultExpenseCategories defaultEarningCategories Dict.empty time time

                        newBooks =
                            Dict.insert id newBook newModel.books
                    in
                    { newModel | books = newBooks } ! [ saveBook (encode 2 (encodeBook newBook)), cmds ]

        ConfirmDeleteBook maybeBook ->
            { model | confirmDeleteBook = True, selectedBookToDelete = maybeBook } ! []

        DeleteBook bookId ->
            let
                newBooks =
                    Dict.remove bookId model.books
            in
            { model | books = newBooks, confirmDeleteBook = False, selectedBookToDelete = Nothing } ! [ deleteBook bookId ]

        CancelDeleteBook ->
            { model | confirmDeleteBook = False, selectedBookToDelete = Nothing } ! []

        DeleteExpenseCategory bookId name ->
            let
                ( newModel, cmds ) =
                    model ! [ getTimeNow ]

                time =
                    newModel.currentTime
            in
            let
                newBooks =
                    Dict.update bookId (deleteCategory Expense name time) newModel.books

                editedBook =
                    Maybe.withDefault (dummyBook (toString <| Dict.size newModel.books)) (Dict.get bookId newBooks)
            in
            { newModel | books = newBooks, currentBook = Just editedBook }
                ! [ saveBook (encode 2 (encodeBook editedBook))
                  , cmds
                  , focusTo "tr-input-category-Expense"
                  ]

        DeleteEarningCategory bookId name ->
            let
                ( newModel, cmds ) =
                    model ! [ getTimeNow ]

                time =
                    newModel.currentTime
            in
            let
                newBooks =
                    Dict.update bookId (deleteCategory Earning name time) newModel.books

                editedBook =
                    Maybe.withDefault (dummyBook (toString <| Dict.size newModel.books)) (Dict.get bookId newBooks)
            in
            { newModel | books = newBooks, currentBook = Just editedBook }
                ! [ saveBook (encode 2 (encodeBook editedBook))
                  , cmds
                  , focusTo "tr-input-category-Earning"
                  ]

        InputCategory categoryType name ->
            case categoryType of
                Expense ->
                    { model | inputExpenseCategory = name } ! []

                Earning ->
                    { model | inputEarningCategory = name } ! []

        AddCategory categoryType ->
            let
                ( newModel, cmds ) =
                    model ! [ getTimeNow ]

                time =
                    newModel.currentTime
            in
            case newModel.currentBook of
                Nothing ->
                    newModel ! [ cmds ]

                Just book ->
                    case categoryType of
                        Expense ->
                            if List.member newModel.inputExpenseCategory book.expenseCategories then
                                newModel ! [ cmds ]
                            else
                                let
                                    newBook =
                                        { book
                                            | expenseCategories = book.expenseCategories ++ [ newModel.inputExpenseCategory ]
                                            , lastEdited = time
                                        }

                                    newBooks =
                                        replace newBook.id newBook newModel.books
                                in
                                { newModel
                                    | books = newBooks
                                    , inputExpenseCategory = ""
                                    , currentBook = Just newBook
                                }
                                    ! [ saveBook (encode 2 (encodeBook newBook))
                                      , cmds
                                      , focusTo "tr-input-category-Expense"
                                      ]

                        Earning ->
                            if List.member newModel.inputEarningCategory book.earningCategories then
                                newModel ! [ cmds ]
                            else
                                let
                                    newBook =
                                        { book
                                            | earningCategories = book.earningCategories ++ [ newModel.inputEarningCategory ]
                                            , lastEdited = time
                                        }

                                    newBooks =
                                        replace newBook.id newBook newModel.books
                                in
                                { newModel
                                    | books = newBooks
                                    , inputEarningCategory = ""
                                    , currentBook = Just newBook
                                }
                                    ! [ saveBook (encode 2 (encodeBook newBook))
                                      , cmds
                                      , focusTo "tr-input-category-Earning"
                                      ]

        ChangeCategoryType ->
            let
                currentBook =
                    Maybe.withDefault (dummyBook "dummy") model.currentBook

                ( categoryType, category ) =
                    case model.selectedCategoryType of
                        Expense ->
                            ( Earning, Maybe.withDefault "Uncategorized" (List.head currentBook.earningCategories) )

                        Earning ->
                            ( Expense, Maybe.withDefault "Uncategorized" (List.head currentBook.expenseCategories) )
            in
            { model | selectedCategoryType = categoryType, selectedCategory = category } ! [ focusTo "tr-input-price" ]

        ChangeCategory name ->
            { model | selectedCategory = name } ! []

        CancelTransactionInput ->
            { model | inputPrice = "", inputDescription = "" } ! [ focusTo "tr-input-price" ]

        InputPrice priceString ->
            { model | inputPrice = priceString } ! []

        InputDescription description ->
            { model | inputDescription = description } ! []

        AddTransaction ->
            let
                ( newModel, cmds ) =
                    model ! [ getTimeNow ]

                time =
                    newModel.currentTime

                resultPrice =
                    model.inputPrice
                        |> String.trim
                        |> String.toFloat

                currentBook =
                    Maybe.withDefault (dummyBook (getId time)) model.currentBook
            in
            case resultPrice of
                Err err ->
                    newModel ! [ cmds, focusTo "tr-input-price" ]

                Ok price ->
                    let
                        realPrice =
                            case newModel.selectedCategoryType of
                                Expense ->
                                    -price

                                Earning ->
                                    price

                        id =
                            getId time

                        newTransaction =
                            Transaction id realPrice newModel.selectedCategory (String.trim newModel.inputDescription) time time

                        newBooks =
                            Dict.update currentBook.id (addTransaction newTransaction) newModel.books

                        newCurrentBook =
                            Dict.get currentBook.id newBooks

                        ( newModel2, cmds2 ) =
                            update (ChangeTransactionsDisplay newModel.currentDisplay)
                                { newModel
                                    | currentBook = newCurrentBook
                                    , books = newBooks
                                    , inputPrice = ""
                                    , inputDescription = ""
                                }
                    in
                    newModel2
                        ! [ saveBook (encode 2 (encodeBook (Maybe.withDefault (dummyBook "dummy") newCurrentBook)))
                          , cmds
                          , cmds2
                          , focusTo "tr-input-price"
                          ]

        ConfirmDeleteTransaction maybeTransaction ->
            { model | confirmDeleteTransaction = True, selectedTransactionToDelete = maybeTransaction } ! []

        DeleteTransaction id ->
            let
                ( newModel, cmds ) =
                    model ! [ getTimeNow ]

                time =
                    newModel.currentTime

                currentBook =
                    Maybe.withDefault (dummyBook (getId time)) model.currentBook

                newTransactions =
                    Dict.remove id currentBook.transactions

                newCurrentBook =
                    { currentBook | transactions = newTransactions, lastEdited = time }

                newBooks =
                    replace currentBook.id newCurrentBook newModel.books

                ( newModel2, cmds2 ) =
                    update (ChangeTransactionsDisplay newModel.currentDisplay)
                        { newModel
                            | books = newBooks
                            , currentBook = Just newCurrentBook
                            , confirmDeleteTransaction = False
                            , selectedTransactionToDelete = Nothing
                        }
            in
            newModel2
                ! [ saveBook (encode 2 (encodeBook newCurrentBook))
                  , cmds
                  , cmds2
                  ]

        CancelDeleteTransaction ->
            { model | confirmDeleteTransaction = False, selectedTransactionToDelete = Nothing } ! []

        CalculateTransactionsToDisplay displayType ->
            let
                currentBook =
                    Maybe.withDefault (dummyBook "dummy") model.currentBook

                trDisplay =
                    calculateTransactionsToDisplay currentBook.transactions displayType
            in
            { model | transactionsToDisplay = trDisplay } ! []

        ChangeTransactionsDisplay displayType ->
            update (CalculateTransactionsToDisplay displayType) { model | currentDisplay = displayType }

        SelectMonth monthString ->
            let
                month =
                    Helper.monthFromString monthString

                displayType =
                    case model.currentDisplay of
                        All ->
                            All

                        ByYear year ->
                            ByYear year

                        ByMonth _ year ->
                            ByMonth month year
            in
            update (ChangeTransactionsDisplay displayType)
                { model
                    | selectedMonth = month
                    , currentDisplay = displayType
                }

        SelectYear yearString ->
            let
                year =
                    case String.toInt yearString of
                        Ok val ->
                            val

                        Err _ ->
                            -999

                displayType =
                    case model.currentDisplay of
                        All ->
                            All

                        ByYear _ ->
                            ByYear year

                        ByMonth month year ->
                            ByMonth month year
            in
            update (ChangeTransactionsDisplay displayType)
                { model
                    | selectedYear = year
                    , currentDisplay = displayType
                }

        InputQuery inputQuery ->
            { model | query = inputQuery } ! []

        ChangeFilterType newFilterType ->
            { model | selectedFilterType = newFilterType } ! []

        FocusOn id ->
            model ! [ Task.attempt FocusResult (Dom.focus id) ]

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    model ! []

                Ok () ->
                    model ! []

        _ ->
            model ! []



---- UTILITY FUNCTIONS ----


addTransaction : Transaction -> Maybe Book -> Maybe Book
addTransaction newTransaction maybeOldBook =
    case maybeOldBook of
        Nothing ->
            Nothing

        Just oldBook ->
            let
                newTransactions =
                    Dict.insert newTransaction.id newTransaction oldBook.transactions
            in
            Just { oldBook | transactions = newTransactions, lastEdited = newTransaction.lastEdited }


deleteCategory : CategoryType -> String -> Time -> Maybe Book -> Maybe Book
deleteCategory category name time maybeBook =
    case maybeBook of
        Nothing ->
            Nothing

        Just book ->
            case category of
                Expense ->
                    let
                        newCategories =
                            List.filter (\n -> n /= name) book.expenseCategories
                    in
                    Just { book | expenseCategories = newCategories, lastEdited = time }

                Earning ->
                    let
                        newCategories =
                            List.filter (\n -> n /= name) book.earningCategories
                    in
                    Just { book | earningCategories = newCategories, lastEdited = time }


getTimeNow : Cmd Msg
getTimeNow =
    Time.now
        |> Task.perform TimeNow


focusTo : String -> Cmd Msg
focusTo elementId =
    Task.perform FocusOn (Task.succeed elementId)


onEscape : Msg -> Attribute Msg
onEscape msg =
    let
        isEscape code =
            if code == 27 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not escape"
    in
    on "keydown" (Json.Decode.andThen isEscape keyCode)


onTab : Msg -> Attribute Msg
onTab msg =
    let
        isTab code =
            if code == 9 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not tab"
    in
    on "keydown" (Json.Decode.andThen isTab keyCode)


salt : Hashids.Context
salt =
    hashidsMinimum "ako ay may lobo" 5


getId : Time -> Id
getId time =
    time
        |> floor
        |> Hashids.encode salt


replace : comparable -> v -> Dict comparable v -> Dict comparable v
replace key newValue dict =
    Dict.remove key dict |> Dict.insert key newValue


calculateTransactionsToDisplay : Dict String Transaction -> TransactionsDisplay -> List Transaction
calculateTransactionsToDisplay allTransactions displayType =
    case displayType of
        All ->
            Dict.values allTransactions

        ByYear year ->
            allTransactions
                |> Dict.values
                |> List.filter (isInTheGivenYear year)

        ByMonth month year ->
            let
                isInTheGivenMonthYear transaction =
                    let
                        transactionMonth =
                            transaction.created
                                |> Date.fromTime
                                |> Date.month
                    in
                    transactionMonth == month && isInTheGivenYear year transaction
            in
            allTransactions
                |> Dict.values
                |> List.filter isInTheGivenMonthYear


isInTheGivenYear : Int -> Transaction -> Bool
isInTheGivenYear year transaction =
    let
        transactionYear =
            transaction.created
                |> Date.fromTime
                |> Date.year
    in
    transactionYear == year



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        heroHidden =
            case model.currentRoute of
                HomeR ->
                    False

                _ ->
                    True
    in
    div []
        [ section [ class "hero is-medium is-dark is-bold" ]
            [ div [ class "hero-head" ]
                [ navbar ]
            , div [ class "hero-body", hidden heroHidden ]
                [ h1 [ class "title is-3" ] [ text "record, monitor, profit" ]
                , h2 [ class "subtitle" ]
                    [ text "record your transactions using this very simple and intuitive app"
                    ]
                , hr [] []
                , button [ class "button is-large is-link is-pulled-right", onClick (NewUrl "/books") ]
                    [ h1 [ class "title is-4 is-light" ]
                        [ text "start app"
                        ]
                    ]
                ]
            , div [ class "hero-foot", hidden heroHidden ] [ div [ class "container" ] [] ]
            ]
        , showcase heroHidden
        , render model
        ]


showcase : Bool -> Html Msg
showcase isHidden =
    div [ class "container is-fluid", hidden isHidden ]
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ article [ class "tr-article" ]
                    [ hr [] []
                    , p [ class "title is-4" ] [ text "Record" ]
                    , p [] [ img [ src "./img/edit.png", alt "record", height 75, width 75 ] [] ]
                    , hr [] []
                    , p [] [ text "Record your transactions to properly monitor your money" ]
                    , hr [] []
                    ]
                ]
            , div [ class "column" ]
                [ article [ class "tr-article" ]
                    [ hr [] []
                    , p [ class "title is-4" ] [ text "Organize" ]
                    , p [] [ img [ src "./img/book.png", alt "organize", height 75, width 75 ] [] ]
                    , hr [] []
                    , p [] [ text "Create multiple books that corresponds to your several accounts" ]
                    , hr [] []
                    ]
                ]
            , div [ class "column" ]
                [ article [ class "tr-article" ]
                    [ hr [] []
                    , p [ class "title is-4" ] [ text "Own your data" ]
                    , p [] [ img [ src "./img/lock.png", alt "Own", height 75, width 75 ] [] ]
                    , hr [] []
                    , p [] [ text "Never worry about privacy because your records are saved in your device" ]
                    , hr [] []
                    ]
                ]
            ]
        ]


navbar : Html Msg
navbar =
    nav [ class "navbar is-dark" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", onClick (NewUrl "/") ]
                [ h1 [ class "title is-light is-3" ] [ text "transaccion" ] ]
            ]
        ]


render : Model -> Html Msg
render model =
    case model.currentRoute of
        HomeR ->
            text ""

        AppR ->
            div []
                [ div [ class "books-content columns is-multiline" ]
                    (List.map (bookCard model.currentTime) (List.reverse <| List.sortBy .lastEdited (Dict.values model.books))
                        ++ [ addBookForm model ]
                    )
                , deleteBookConfirmation model.selectedBookToDelete model.confirmDeleteBook

                -- , a [ class "float" ] [ i [ class "fa fa-plus my-float" ] [] ]
                ]

        BookR id ->
            let
                maybeBook =
                    Dict.get id model.books
            in
            case maybeBook of
                Just book ->
                    div [ class "books-content block" ]
                        [ div [ class "columns" ]
                            [ div [ class "column is-7" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-3" ]
                                        [ h1 [ class "title is-3" ] [ text book.name ] ]
                                    , div [ class "column" ] [ transactionInputField model ]
                                    ]

                                -- , hr [] []
                                , transactionsTable model
                                ]
                            , div [ class "column is-5" ]
                                [ summaryBoxByCategory model.transactionsToDisplay model.currentDisplay
                                , summaryBoxByMonth model.currentTime (book.transactions |> Dict.values)
                                , chartBox model.transactionsToDisplay
                                , categoriesBox book Expense DeleteExpenseCategory
                                , categoriesBox book Earning DeleteEarningCategory
                                ]
                            ]
                        , deleteTransactionConfirmation model.selectedTransactionToDelete model.confirmDeleteTransaction
                        ]

                Nothing ->
                    div [ class "block" ]
                        [ text ("There is no book with that id: " ++ toString id) ]

        PageNotFoundR ->
            div [] [ text "The page is not available." ]


deletableTags : String -> (String -> Msg) -> List String -> List (Html Msg) -> Html Msg
deletableTags color xMessage names addons =
    div [ class "field is-grouped is-grouped-multiline" ]
        ((names
            |> List.map (xTag color xMessage)
         )
            ++ addons
        )


xTag : String -> (String -> Msg) -> String -> Html Msg
xTag color xMessage name =
    div [ class "control" ]
        [ div [ class "tags has-addons" ]
            [ span [ class ("tag " ++ color) ] [ text name ]
            , a [ class "tag is-delete", onClick (xMessage name) ] []
            ]
        ]


categoriesBox : Book -> CategoryType -> (Id -> String -> Msg) -> Html Msg
categoriesBox book category msg =
    let
        ( color, categories, name ) =
            if category == Expense then
                ( "is-danger", book.expenseCategories, "Expense Categories" )
            else
                ( "is-link", book.earningCategories, "Earning Categories" )
    in
    div [ class "box" ]
        [ article [ class "media" ]
            [ div [ class "media-content" ]
                [ div
                    [ class "content" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-two-thirds" ]
                            [ p [ class "subtitle is-5" ] [ text name ] ]
                        ]

                    -- , hr [] []
                    , deletableTags color (msg book.id) categories [ addCategoryForm book category ]
                    ]
                ]
            ]
        ]


summaryBoxByCategory : List Transaction -> TransactionsDisplay -> Html Msg
summaryBoxByCategory transactions currentDisplay =
    let
        expenses =
            transactions
                |> List.filter (\t -> t.price < 0)
                |> List.map (\e -> { e | price = -e.price })

        earnings =
            transactions
                |> List.filter (\t -> t.price >= 0)

        summaryText =
            case currentDisplay of
                All ->
                    "Summary: All"

                ByYear year ->
                    "Summary: " ++ toString year

                ByMonth month year ->
                    "Summary: " ++ toString month ++ toString year
    in
    div [ class "box" ]
        [ div [ class "content" ]
            [ h1 [ class "subtitle is-5", style [ ( "float", "left" ) ] ] [ strong [] [ text summaryText ] ]
            , p [ style [ ( "float", "right" ) ] ]
                [ span [] [ text "Current " ]
                , currentBalance transactions
                ]
            , br [] []
            ]
        , summaryTable Expense expenses
        , summaryTable Earning earnings
        ]


currentBalance : List Transaction -> Html Msg
currentBalance transactions =
    let
        total =
            transactions
                |> List.map .price
                |> List.sum

        ( theClass, theText ) =
            if total < 0 then
                ( "tr-expense tr-balance", Round.round 2 -total )
            else
                ( "tr-earning tr-balance", Round.round 2 total )
    in
    span [ class theClass ] [ text theText ]


summaryTable : CategoryType -> List Transaction -> Html Msg
summaryTable categoryType transactions =
    let
        categories =
            transactions
                |> List.map .category
                |> unique

        totalPrices =
            categories
                |> List.map (calcTotalPrice transactions)

        percentages =
            totalPrices
                |> List.map (\p -> p / overallPrice * 100)

        overallPrice =
            totalPrices
                |> List.sum

        ( priceHeadingText, priceHeadingClass ) =
            case categoryType of
                Expense ->
                    ( overallPrice |> Helper.toTwoDecimal, "tr-expense" )

                Earning ->
                    ( overallPrice |> Helper.toTwoDecimal, "tr-earning" )

        summaryHeader =
            thead []
                [ tr []
                    [ th [] [ text (toString categoryType ++ "s") ]
                    , th [] [ p [ align "right" ] [ text "Percent" ] ]
                    , th [] [ p [ class priceHeadingClass, align "right" ] [ text priceHeadingText ] ]
                    ]
                ]

        rows =
            List.map3 (summaryRow categoryType) categories totalPrices percentages

        theTable =
            case List.length rows of
                0 ->
                    div [] [ text ("No inputted " ++ String.toLower (toString categoryType ++ "s") ++ ".") ]

                _ ->
                    summaryHeader
                        :: [ tbody [] rows ]
                        |> table [ class "table is-hoverable is-fullwidth" ]
    in
    div [ class "box" ] [ theTable ]


summaryRow : CategoryType -> String -> Float -> Float -> Html Msg
summaryRow categoryType category total percent =
    let
        ( priceText, priceClass ) =
            case categoryType of
                Expense ->
                    ( Helper.toTwoDecimal total, "tr-expense" )

                Earning ->
                    ( Helper.toTwoDecimal total, "tr-earning" )
    in
    tr []
        [ td [] [ text category ]
        , td [] [ p [ align "right" ] [ text (percent |> Helper.toTwoDecimal) ] ]
        , td [] [ p [ class priceClass, align "right" ] [ text priceText ] ]
        ]


calcTotalPrice : List Transaction -> String -> Float
calcTotalPrice transactions category =
    let
        validTransactions =
            List.filter (\transaction -> transaction.category == category) transactions
    in
    List.foldl (+) 0 (List.map .price validTransactions)


chartBox : List Transaction -> Html Msg
chartBox transactions =
    let
        expenses =
            transactions
                |> List.filter (\t -> t.price < 0)
                |> List.map (\e -> { e | price = -e.price })

        toDisplay =
            case expenses of
                [] ->
                    text "Nothing to display."

                _ ->
                    chartExpense expenses
    in
    div [ class "box" ]
        [ h1 [ class "subtitle is-5" ] [ strong [] [ text "Expense Chart" ] ]
        , div [] [ toDisplay ]
        ]


summaryBoxByMonth : Time -> List Transaction -> Html Msg
summaryBoxByMonth currentTime transactions =
    let
        currentYear =
            currentTime |> (Date.fromTime >> Date.year)

        currentYearTransactions =
            transactions
                |> List.filter (\t -> (t.created |> (Date.fromTime >> Date.year)) == currentYear)

        summaryHeader =
            thead []
                [ tr []
                    [ th [] [ text "Month" ]
                    , th [] [ p [ align "right" ] [ text "Expenses" ] ]
                    , th [] [ p [ align "right" ] [ text "Earnings" ] ]
                    , th [] [ p [ align "right" ] [ text "Total" ] ]
                    ]
                ]

        months =
            currentYearTransactions
                |> List.map .created
                |> List.map (Date.fromTime >> Date.month)
                |> List.map toString
                |> unique
                |> List.map Helper.monthFromString

        calcTotalByMonth transactions month =
            transactions
                |> List.filter
                    (\t ->
                        month == (t.created |> (Date.fromTime >> Date.month))
                    )
                |> List.map .price
                |> List.sum

        totalPrices =
            months
                |> List.map (calcTotalByMonth currentYearTransactions)

        expenseTransactions =
            List.filter (\t -> t.price < 0) currentYearTransactions

        earningTransactions =
            List.filter (\t -> t.price >= 0) currentYearTransactions

        totalExpenses =
            months
                |> List.map (calcTotalByMonth expenseTransactions)

        totalEarnings =
            months
                |> List.map (calcTotalByMonth earningTransactions)

        rows =
            List.map4 summaryByMonthRow months totalExpenses totalEarnings totalPrices

        theTable =
            case List.length rows of
                0 ->
                    div [] [ text "Nothing to display..." ]

                _ ->
                    summaryHeader
                        :: [ tbody [] rows ]
                        |> table [ class "table is-hoverable is-fullwidth" ]

        title =
            "Year " ++ toString currentYear
    in
    div [ class "box" ]
        [ div [ class "content" ]
            [ h1 [ class "subtitle is-5", style [ ( "float", "left" ) ] ] [ strong [] [ text title ] ]
            , p [ style [ ( "float", "right" ) ] ]
                [ span [] [ text "Overall " ]
                , currentBalance currentYearTransactions
                ]
            , br [] []
            ]
        , div [ class "box" ] [ theTable ]
        ]


summaryByMonthRow : Month -> Float -> Float -> Float -> Html Msg
summaryByMonthRow month expenses earnings totalPrice =
    let
        ( priceText, priceClass ) =
            if totalPrice < 0 then
                ( -totalPrice |> Helper.toTwoDecimal, "tr-expense" )
            else
                ( totalPrice |> Helper.toTwoDecimal, "tr-earning" )
    in
    tr []
        [ td [] [ text (toString month) ]
        , td [] [ p [ class "tr-expense", align "right" ] [ text (-expenses |> Helper.toTwoDecimal) ] ]
        , td [] [ p [ class "tr-earning", align "right" ] [ text (earnings |> Helper.toTwoDecimal) ] ]
        , td [] [ p [ class priceClass, align "right" ] [ text priceText ] ]
        ]


chartExpense : List Transaction -> Html Msg
chartExpense expenses =
    let
        categories =
            expenses
                |> List.map .category
                |> unique

        prices =
            categories
                |> List.map (calcTotalPrice expenses)

        pairs =
            List.map2 (\category price -> ( price, category )) categories prices
    in
    pairs
        |> C.pie
        |> C.toHtml


addCategoryForm : Book -> CategoryType -> Html Msg
addCategoryForm book categoryType =
    span [ style [ ( "font-size", "1rem" ), ( "font-weight", "normal" ), ( "width", "100px" ) ] ]
        [ Html.form [ class "field has-addons", onSubmit (AddCategory categoryType) ]
            [ div [ class "control" ]
                [ input
                    [ class "input is-small"
                    , id ("tr-input-category-" ++ toString categoryType)
                    , type_ "text"
                    , placeholder "new"
                    , minlength 1
                    , maxlength 15
                    , onInput (InputCategory categoryType)
                    ]
                    []
                ]
            , div [ class "control" ]
                [ button [ class "button is-small is-dark", type_ "submit" ]
                    [ span [ class "icon" ] [ i [ class "fa fa-plus" ] [] ] ]
                ]
            ]
        ]


bookCard : Time -> Book -> Html Msg
bookCard currentTime book =
    let
        created =
            book.created
                |> Date.fromTime
                |> Date.toFormattedString "EEE, d-MMM-yy, HH:mm"

        lastEdited =
            book.lastEdited
                |> Date.fromTime
                |> Date.toFormattedString "EEE, d-MMM-yy, HH:mm"

        currentMonth =
            currentTime
                |> Date.fromTime
                |> Date.month

        currentYear =
            currentTime
                |> Date.fromTime
                |> Date.year

        recentTransactions =
            let
                isInTheGivenMonthYear transaction =
                    let
                        transactionMonth =
                            transaction.created
                                |> Date.fromTime
                                |> Date.month
                    in
                    transactionMonth == currentMonth && isInTheGivenYear currentYear transaction
            in
            book.transactions
                |> Dict.values
                |> List.filter isInTheGivenMonthYear

        totalExpenses =
            recentTransactions
                |> List.map .price
                |> List.filter (\price -> price < 0)
                |> List.map (\price -> -price)
                |> List.sum

        totalEarnings =
            recentTransactions
                |> List.map .price
                |> List.filter (\price -> price >= 0)
                |> List.sum

        ( balance, balanceClass ) =
            let
                bal =
                    totalEarnings - totalExpenses

                cls =
                    if bal < 0 then
                        "tr-expense"
                    else
                        "tr-earning"
            in
            ( abs bal, cls )

        ( percent, progressClass ) =
            let
                ratio =
                    totalExpenses / totalEarnings
            in
            if ratio < 1 then
                ( ratio * 100, "is-link" )
            else
                ( 100, "is-danger" )

        percentString =
            percent
                |> round
                |> toString

        isHidden =
            totalEarnings == 0 && totalExpenses == 0
    in
    div [ class "column is-3" ]
        [ div [ class "card" ]
            [ header [ class "card-header" ]
                [ p [ class "card-header-title" ]
                    [ text book.name
                    , icon "fa-book" "is-left"
                    ]
                ]
            , div [ class "card-content" ]
                [ div [ class "content" ]
                    [ text "Recent month's balance: "
                    , strong [ class balanceClass ] [ text (Helper.toTwoDecimal balance) ]
                    , br [] []
                    , div [ hidden isHidden ]
                        [ br [] []
                        , node "progress"
                            [ class ("progress is-medium " ++ progressClass)
                            , attribute "value" percentString
                            , attribute "max" "100"
                            ]
                            []
                        ]
                    , br [ hidden isHidden ] []
                    , node "time" [] [ text ("Last Edited: " ++ lastEdited) ]
                    ]
                ]
            , footer [ class "card-footer" ]
                [ a [ class "card-footer-item tr-book-delete", onClick (ConfirmDeleteBook (Just book)) ] [ text "Delete", icon "fa-remove" "" ]
                , div [ class "card-footer-item", onClick (SelectBook book.id) ]
                    [ a [ onClick (NewUrl ("/books/" ++ book.id)) ] [ text "Open ", icon "fa-folder-open" "" ] ]
                ]
            ]
        ]


deleteBookConfirmation : Maybe Book -> Bool -> Html Msg
deleteBookConfirmation maybeBook isActive =
    let
        modalClass =
            case maybeBook of
                Nothing ->
                    "modal"

                Just book ->
                    if isActive then
                        "modal is-active"
                    else
                        "modal"

        book =
            Maybe.withDefault (dummyBook "dummy") maybeBook

        message =
            "Are you sure you want to delete this book?"

        bookName =
            book.name
    in
    div [ class modalClass ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ] [ text "delete confirmation" ]
                , button [ class "delete", attribute "aria-label" "close", onClick CancelDeleteBook ] []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "content" ]
                    [ p [] [ text message ]
                    , h1 [ class "title is-5", align "center" ] [ text bookName ]
                    ]
                ]
            , footer [ class "modal-card-foot", style [ ( "justify-content", "flex-end" ) ] ]
                [ button [ class "button is-pulled-right", onClick CancelDeleteBook ] [ text "Cancel" ]
                , button [ class "button is-danger is-pulled-right", onClick (DeleteBook book.id) ] [ text "Delete" ]
                ]
            ]
        ]


deleteTransactionConfirmation : Maybe Transaction -> Bool -> Html Msg
deleteTransactionConfirmation maybeTransaction isActive =
    let
        modalClass =
            case maybeTransaction of
                Nothing ->
                    "modal"

                Just _ ->
                    if isActive then
                        "modal is-active"
                    else
                        "modal"

        transaction =
            Maybe.withDefault (Transaction "" 0.0 "" "" 0.0 0.0) maybeTransaction

        message =
            "Are you sure you want to delete this transaction?"

        ( priceText, priceClass ) =
            if transaction.price > 0 then
                ( Helper.toTwoDecimal transaction.price, "tr-earning" )
            else
                ( Helper.toTwoDecimal -transaction.price, "tr-expense" )

        transactionText =
            [ span [ class priceClass ] [ text priceText ]
            , text (" | " ++ transaction.category ++ " | ")
            , span [ class "tr-description" ] [ text transaction.description ]
            ]
    in
    div [ class modalClass ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ] [ text "delete confirmation" ]
                , button [ class "delete", attribute "aria-label" "close", onClick CancelDeleteTransaction ] []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "content" ]
                    [ p [] [ text message ]
                    , h1 [ class "title is-5", align "center" ] transactionText
                    ]
                ]
            , footer [ class "modal-card-foot", style [ ( "justify-content", "flex-end" ) ] ]
                [ button [ class "button is-pulled-right", onClick CancelDeleteTransaction ] [ text "Cancel" ]
                , button [ class "button is-danger is-pulled-right", onClick (DeleteTransaction transaction.id) ] [ text "Delete" ]
                ]
            ]
        ]


addBookForm : Model -> Html Msg
addBookForm model =
    Html.form [ class "column is-3", onSubmit (AddBook model.inputBookName) ]
        [ div [ class "field has-addons" ]
            [ p
                [ class "control has-icons-left tooltip is-tooltip-multiline"
                , attribute "data-tooltip" "Your transactions are organized by 'record book' or 'book'."
                ]
                [ input
                    [ class "input is-medium"
                    , type_ "text"
                    , placeholder "add new book"
                    , onInput InputBookName
                    ]
                    []
                , icon "fa-book" "is-left"
                ]
            , p [ class "control" ]
                [ button [ class "button is-dark is-medium", type_ "submit" ]
                    [ span [ class "icon" ]
                        [ i [ class "fa fa-plus" ] [] ]
                    ]
                ]
            ]
        ]


transactionsTable : Model -> Html Msg
transactionsTable model =
    let
        transactions =
            model.transactionsToDisplay
                |> List.sortBy .created
                |> List.reverse
                |> List.filter (byQuery (String.trim model.query) model.selectedFilterType)

        byQuery query filterType transaction =
            let
                inDescription =
                    transaction.description
                        |> String.toLower
                        |> String.contains (String.toLower query)

                inCategory =
                    transaction.category
                        |> String.toLower
                        |> String.contains (String.toLower query)

                inFilterType =
                    if transaction.price > 0 then
                        case filterType of
                            "Expenses" ->
                                False

                            "Earnings" ->
                                True

                            _ ->
                                True
                    else
                        case filterType of
                            "Expenses" ->
                                True

                            "Earnings" ->
                                False

                            _ ->
                                True
            in
            if (inDescription || inCategory) && inFilterType then
                True
            else
                False

        currentBook =
            Maybe.withDefault (dummyBook "dummy") model.currentBook

        listCreated =
            currentBook.transactions
                |> Dict.values
                |> List.map .created
                |> List.map Date.fromTime

        listYear =
            listCreated
                |> List.map Date.year
                |> unique
                |> List.map toString

        listMonth =
            listCreated
                |> List.map Date.month
                |> List.map toString
                |> unique

        transactionTableTemplate content =
            div []
                [ h1 [ class "subtitle" ] [ strong [] [ text "Transactions" ] ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ displayTransactionsControl listMonth listYear model.selectedMonth model.selectedYear model.currentDisplay ]
                    , div [ class "column" ]
                        [ div [ class "field has-addons" ]
                            [ p [ class "control" ]
                                [ div [ class "select" ]
                                    [ select [ onInput ChangeFilterType ]
                                        (List.map (nameToOptionSelected model.selectedFilterType)
                                            [ "Any", "Expenses", "Earnings" ]
                                        )
                                    ]
                                ]
                            , p [ class "control has-icons-left" ]
                                [ input
                                    [ class "input"
                                    , type_ "text"
                                    , placeholder "filter transactions"
                                    , onInput InputQuery
                                    , onTab (FocusOn "tr-input-price")
                                    ]
                                    []
                                , icon "fa-search" "is-left"
                                ]
                            ]
                        ]
                    ]
                , content
                ]

        content =
            case transactions of
                [] ->
                    div [ class "content" ]
                        [ p []
                            [ text "Nothing. Enter different search strings." ]
                        ]

                _ ->
                    table [ class "table is-hoverable is-fullwidth" ]
                        [ tbody []
                            (transactions
                                |> List.map listTransaction
                            )
                        ]

        totalPrice =
            transactions
                |> List.map .price
                |> List.sum

        ( totalPriceClass, totalPriceText ) =
            if totalPrice < 0 then
                ( "tr-expense", -totalPrice |> Helper.toTwoDecimal )
            else
                ( "tr-earning", totalPrice |> Helper.toTwoDecimal )
    in
    div [ class "box" ]
        [ transactionTableTemplate content
        , hr [] []
        , div []
            [ p [ style [ ( "text-align", "right" ) ] ]
                [ text "Total | "
                , span [ class totalPriceClass ] [ text totalPriceText ]
                ]
            ]
        ]


listTransaction : Transaction -> Html Msg
listTransaction transaction =
    let
        month =
            transaction.created
                |> Date.fromTime
                |> Date.toFormattedString "MMM"

        day =
            transaction.created
                |> Date.fromTime
                |> Date.toFormattedString "dd"

        ( priceText, transactionType ) =
            if transaction.price < 0 then
                ( Helper.toTwoDecimal -transaction.price, Expense )
            else
                ( Helper.toTwoDecimal transaction.price, Earning )

        category =
            transaction.category

        description =
            case transaction.description of
                "" ->
                    "(no description)"

                _ ->
                    transaction.description

        priceClass =
            case transactionType of
                Expense ->
                    "tr-expense"

                Earning ->
                    "tr-earning"
    in
    tr []
        [ td [ class "tr-center" ]
            [ p [ class "tr-day", align "center" ] [ text day ]
            , p [ class "tr-month", align "center" ] [ text month ]
            ]
        , td [ class "tr-left" ]
            [ p []
                [ span [ class priceClass ] [ text priceText ]
                , span [ class "tr-separator" ] [ text " | " ]
                , span [ class "tr-category" ] [ text category ]
                ]
            , p [ class "tr-description" ] [ text description ]
            ]
        , td [ class "tr-right" ]
            [ button [ class "delete is-medium", onClick (ConfirmDeleteTransaction (Just transaction)) ] []
            ]
        ]


transactionInputField : Model -> Html Msg
transactionInputField model =
    let
        ( categories, categoryTypeColor, categoryTypeTooltip, inputPriceTooltip ) =
            case model.selectedCategoryType of
                Expense ->
                    ( .expenseCategories (Maybe.withDefault (dummyBook "dummy") model.currentBook)
                    , "is-danger"
                    , "Click to change to Earning."
                    , "How much did you spend?"
                    )

                Earning ->
                    ( .earningCategories (Maybe.withDefault (dummyBook "dummy") model.currentBook)
                    , "is-link"
                    , "Click to change to Expense."
                    , "How much did you earn?"
                    )

        inputPriceClass =
            let
                resPrice =
                    model.inputPrice
                        |> String.trim
                        |> String.toFloat
            in
            case resPrice of
                Ok _ ->
                    case model.selectedCategoryType of
                        Expense ->
                            "input is-danger"

                        Earning ->
                            "input is-link"

                Err _ ->
                    "input is-warning"

        categoryTypeButtonText =
            case model.selectedCategoryType of
                Expense ->
                    text "Expense"

                Earning ->
                    text "Earning"

        -- span []
        --     [ span [ class "icon" ]
        --         [ i [ class "fa fa-minus" ] [] ]
        --     ]
        -- span []
        --     [ span [ class "icon" ]
        --         [ i [ class "fa fa-plus" ] [] ]
        --     ]
    in
    Html.form [ class "field tr-input-field", onSubmit AddTransaction, onEscape CancelTransactionInput ]
        [ div [ class "field has-addons" ]
            [ p
                [ class "control tooltip"
                , attribute "data-tooltip" categoryTypeTooltip
                ]
                [ a
                    [ class ("button " ++ categoryTypeColor)
                    , onClick ChangeCategoryType
                    ]
                    [ categoryTypeButtonText ]
                ]
            , p
                [ class "control has-icons-left tooltip"
                , attribute "data-tooltip" inputPriceTooltip
                ]
                [ input
                    [ class inputPriceClass
                    , id "tr-input-price"
                    , type_ "text"
                    , maxlength 15
                    , value model.inputPrice
                    , onInput InputPrice
                    , placeholder "price"
                    , style [ ( "width", "100px" ) ]

                    -- , attribute "min" "0"
                    -- , attribute "step" "0.01"
                    ]
                    []
                , icon "fa-money" "is-left"
                ]
            , div
                [ class "control tooltip"
                , attribute "data-tooltip" "Edit the categories in the lower right boxes."
                ]
                [ div
                    [ class "select"
                    ]
                    [ select [ onInput ChangeCategory ]
                        (List.map (nameToOptionSelected model.selectedCategory) categories)
                    ]
                ]
            , div
                [ class "control tooltip is-tooltip-multiline"
                , attribute "data-tooltip" "Perhaps tell me about the product name, quantity, brand and shop, if not too much to ask."
                ]
                [ input
                    [ class "input is-fullwidth"
                    , type_ "text"
                    , placeholder "description"
                    , maxlength 150
                    , value model.inputDescription
                    , onInput InputDescription
                    , onTab (FocusOn "tr-input-price")

                    -- , style [ ( "width", "200px" ) ]
                    ]
                    []
                ]
            , div
                [ class "control tooltip"
                , attribute "data-tooltip" "Press <enter> to confirm transaction. Press <esc> to cancel."
                ]
                [ button
                    [ class "button is-dark"
                    , type_ "submit"
                    ]
                    [ icon "fa-chevron-right" "" ]
                ]
            , div
                [ class "control tooltip"
                , attribute "data-tooltip" "Press <esc> to cancel."
                , hidden True
                ]
                [ button
                    [ class "button is-danger"
                    , onClick CancelTransactionInput
                    ]
                    [ icon "fa-close" "" ]
                ]
            ]
        ]


icon : String -> String -> Html Msg
icon name additionalAttributes =
    span [ class ("icon " ++ additionalAttributes) ]
        [ i [ class ("fa " ++ name) ] [] ]


nameToOptionSelected : String -> String -> Html Msg
nameToOptionSelected selectedName name =
    if selectedName == name then
        option [ value name, selected True ] [ text name ]
    else
        nameToOption name


nameToOption : String -> Html Msg
nameToOption name =
    option [ value name ] [ text name ]


displayTransactionsControl : List String -> List String -> Month -> Int -> TransactionsDisplay -> Html Msg
displayTransactionsControl months years selectedMonth selectedYear currentDisplay =
    let
        newTransactionDisplay currentDisplay =
            case currentDisplay of
                All ->
                    ChangeTransactionsDisplay (ByMonth selectedMonth selectedYear)

                ByMonth _ _ ->
                    ChangeTransactionsDisplay (ByYear selectedYear)

                ByYear _ ->
                    ChangeTransactionsDisplay All

        displayButtonText currentDisplay =
            case currentDisplay of
                All ->
                    "Show All"

                ByMonth _ _ ->
                    "By Month"

                ByYear _ ->
                    "By Year"

        tooltipText currentDisplay =
            case currentDisplay of
                All ->
                    "Click to change display to: By Month"

                ByMonth _ _ ->
                    "Click to change display to: By Year"

                ByYear _ ->
                    "Click to change display to: Show All"

        ( isMonthDisabled, isYearDisabled ) =
            case currentDisplay of
                All ->
                    ( True, True )

                ByMonth _ _ ->
                    ( False, False )

                ByYear _ ->
                    ( True, False )
    in
    div [ class "field has-addons" ]
        [ div
            [ class "control tooltip"
            , attribute "data-tooltip" (tooltipText currentDisplay)
            ]
            [ button
                [ class "button is-dark"
                , style [ ( "width", "100px" ) ]
                , onClick (newTransactionDisplay currentDisplay)
                ]
                [ text (displayButtonText currentDisplay) ]
            ]
        , div [ class "control" ]
            [ div [ class "select" ]
                [ select
                    [ disabled isMonthDisabled
                    , onInput SelectMonth
                    ]
                    (List.map (nameToOptionSelected (toString selectedMonth)) months)
                ]
            ]
        , div [ class "control" ]
            [ div [ class "select" ]
                [ select
                    [ disabled isYearDisabled
                    , onInput SelectYear
                    ]
                    (List.map (nameToOptionSelected (toString selectedYear)) years)
                ]
            ]
        ]
