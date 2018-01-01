port module Main exposing (..)

-- elm-package install --yes elm-community/json-extra

import Date exposing (Date, Month(..))
import Date.Extra as Date
import Dict exposing (Dict)
import Dom
import Hashids exposing (hashidsMinimum)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Json_ exposing (decodeBooks, encodeBook, encodeBooks)
import List.Extra exposing (unique)
import Model exposing (..)
import Navigation as Nav
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
            Model HomeR initBooks "" "" "" Nothing 0 "" Expense "" "" ! [ getTimeNow ]

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
            Model HomeR books "" "" "" Nothing 0 "" Expense "" "" ! [ getTimeNow ]


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
    | DeleteTransaction Id
    | CancelTransactionInput
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
            model ! [ Nav.newUrl url ]

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
                cBook =
                    Maybe.withDefault (dummyBook "dummy") (Dict.get bookId model.books)

                category =
                    case model.selectedCategoryType of
                        Expense ->
                            Maybe.withDefault "Uncategorized" (List.head cBook.expenseCategories)

                        Earning ->
                            Maybe.withDefault "Uncategorized" (List.head cBook.earningCategories)
            in
            { model | selectedCategory = category, currentBook = Just cBook } ! [ focusTo "tr-input-price" ]

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

        DeleteBook bookId ->
            let
                newBooks =
                    Dict.remove bookId model.books
            in
            { model | books = newBooks } ! [ deleteBook bookId ]

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
                  , focusTo "tr-input-price"
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
                  , focusTo "tr-input-price"
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
                    String.toFloat model.inputPrice

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
                            Transaction id realPrice newModel.selectedCategory newModel.inputDescription time time

                        newBooks =
                            Dict.update currentBook.id (addTransaction newTransaction) newModel.books

                        newCurrentBook =
                            Dict.get currentBook.id newBooks
                    in
                    { newModel
                        | currentBook = newCurrentBook
                        , books = newBooks
                        , inputPrice = ""
                        , inputDescription = ""
                    }
                        ! [ saveBook (encode 2 (encodeBook (Maybe.withDefault (dummyBook "dummy") newCurrentBook)))
                          , cmds
                          , focusTo "tr-input-price"
                          ]

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
            in
            { newModel | books = newBooks, currentBook = Just newCurrentBook }
                ! [ saveBook (encode 2 (encodeBook newCurrentBook))
                  , cmds
                  , focusTo "tr-input-price"
                  ]

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
                [ div [ class "container" ]
                    [ h1 [ class "title" ] [ text "record, monitor, profit" ]
                    , h2 [ class "subtitle" ]
                        [ text "record your transactions using this very simple and intuitive app"
                        ]
                    ]
                ]
            , div [ class "hero-foot", hidden heroHidden ] [ div [ class "container" ] [] ]
            ]
        , render model
        ]


navbar : Html Msg
navbar =
    nav [ class "navbar is-dark" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", onClick (NewUrl "/") ]
                [ h1 [ class "title is-light is-3" ] [ text "transaccion" ] ]
            , a [ class "navbar-item", onClick (NewUrl "/books") ]
                [ h1 [ class "subtitle is-4 is-light" ]
                    [ text "view books"
                    , span [ class "icon" ] [ i [ class "fa fa-angle-double-right" ] [] ]
                    ]
                ]
            ]
        ]


render : Model -> Html Msg
render model =
    case model.currentRoute of
        HomeR ->
            text ""

        AppR ->
            div [ class "books-content columns is-multiline" ]
                (List.map bookCard (List.reverse <| List.sortBy .lastEdited (Dict.values model.books))
                    ++ [ addBookForm model ]
                )

        BookR id ->
            let
                maybeBook =
                    Dict.get id model.books
            in
            case maybeBook of
                Just book ->
                    div [ class "books-content block" ]
                        [ div [ class "columns" ]
                            [ div [ class "column is-two-thirds" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-3" ]
                                        [ h1 [ class "title is-3" ] [ text book.name ] ]
                                    , div [ class "column" ] [ transactionInputField model ]
                                    ]
                                , hr [] []
                                , transactionsTable model
                                ]
                            , div [ class "column is-one-third" ]
                                [ summaryBox book
                                , chartBox book
                                , categoriesBox book Expense DeleteExpenseCategory
                                , categoriesBox book Earning DeleteEarningCategory
                                ]
                            ]
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
                ( "is-link", book.expenseCategories, "Expense Categories" )
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


summaryBox : Book -> Html Msg
summaryBox book =
    div [ class "box" ]
        [ h1 [ class "subtitle is-5" ] [ text "Summary" ]

        -- , hr [] []
        , summaryTable (Dict.values book.transactions)
        ]


summaryTable : List Transaction -> Html Msg
summaryTable transactions =
    summaryList transactions


summaryList : List Transaction -> Html Msg
summaryList transactions =
    let
        rows =
            transactions
                |> summarize
                |> List.sortBy .category
                |> List.map summaryRow

        summaryHeader =
            thead []
                [ tr []
                    [ th [] [ text "category" ]
                    , th [] [ text "total price" ]
                    , th [] [ text "in chart" ]
                    ]
                ]
    in
    case List.length rows of
        0 ->
            div [] [ text "No inputted transactions." ]

        _ ->
            summaryHeader
                :: [ tbody [] rows ]
                |> table [ class "table is-hoverable is-fullwidth" ]


summaryRow : SummaryByCategory -> Html Msg
summaryRow categorySummary =
    let
        ( priceText, transactionType ) =
            if categorySummary.totalPrice < 0 then
                ( "(" ++ toString -categorySummary.totalPrice ++ ")", Expense )
            else
                ( toString categorySummary.totalPrice, Earning )

        priceClass =
            case transactionType of
                Expense ->
                    "tr-price"

                Earning ->
                    "tr-earning"
    in
    tr []
        [ td [] [ text categorySummary.category ]
        , td [ class priceClass ] [ text priceText ]
        , td []
            [ input
                [ type_ "checkbox"

                -- , onClick (ToggleChartInclude categorySummary)
                , id categorySummary.category
                , checked categorySummary.includedInChart
                ]
                []
            , label [ for categorySummary.category ] []
            ]
        ]


summarize : List Transaction -> List SummaryByCategory
summarize transactions =
    case transactions of
        [] ->
            []

        _ ->
            let
                categories =
                    transactions
                        |> List.map (\transaction -> transaction.category)
                        |> unique

                totalPrices =
                    categories
                        |> List.map (calcTotalPrice transactions)
            in
            List.map2 (SummaryByCategory True) categories totalPrices


calcTotalPrice : List Transaction -> String -> Float
calcTotalPrice transactions category =
    let
        validTransactions =
            List.filter (\transaction -> transaction.category == category) transactions
    in
    List.foldl (+) 0 (List.map .price validTransactions)


chartBox : Book -> Html Msg
chartBox book =
    div [ class "box" ]
        [ h1 [ class "subtitle is-5" ] [ text "Chart" ]
        , div [] [ text "Sorry, not yet implemented..." ]

        -- , hr [] []
        ]


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


bookCard : Book -> Html Msg
bookCard book =
    let
        created =
            book.created
                |> Date.fromTime
                |> Date.toFormattedString "EEE, d-MMM-yy, HH:mm"

        lastEdited =
            book.lastEdited
                |> Date.fromTime
                |> Date.toFormattedString "EEE, d-MMM-yy, HH:mm"
    in
    div [ class "column is-3" ]
        [ div [ class "card" ]
            [ header [ class "card-header" ]
                [ p [ class "card-header-title" ]
                    [ text book.name
                    , span [ class "icon is-left" ]
                        [ i [ class "fa fa-book" ] [] ]
                    ]
                ]
            , div [ class "card-content" ]
                [ div [ class "content" ]
                    [ node "time" [] [ text ("Last Edited: " ++ lastEdited) ]
                    ]
                ]
            , footer [ class "card-footer" ]
                [ a [ class "card-footer-item", onClick (DeleteBook book.id) ] [ text "Delete" ]
                , div [ class "card-footer-item", onClick (SelectBook book.id) ]
                    [ a [ onClick (NewUrl ("/books/" ++ book.id)) ] [ text "Open" ] ]
                ]
            ]
        ]


addBookForm : Model -> Html Msg
addBookForm model =
    Html.form [ class "column is-3", onSubmit (AddBook model.inputBookName) ]
        [ div [ class "field has-addons" ]
            [ p [ class "control has-icons-left" ]
                [ input [ class "input is-medium", type_ "text", placeholder "add new book", onInput InputBookName ] []
                , span [ class "icon is-left" ]
                    [ i [ class "fa fa-book" ] [] ]
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
    case model.currentBook of
        Nothing ->
            text "No selected book."

        Just currentBook ->
            let
                transactions =
                    currentBook.transactions
                        |> Dict.values
                        |> List.sortBy .created
                        |> List.reverse
            in
            case transactions of
                [] ->
                    div [ class "column" ] [ text "You have no transactions to display :(" ]

                _ ->
                    div [ class "column" ]
                        [ h1 [ class "subtitle" ] [ text "List of transactions" ]

                        -- , hr [] []
                        , table [ class "table is-hoverable is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text "date" ]
                                    , th [] [ text "price" ]
                                    , th [] [ text "category" ]
                                    , th [] [ text "description" ]
                                    , th [] []
                                    ]
                                ]
                            , tbody []
                                (transactions
                                    |> List.map listTransaction
                                )
                            ]
                        ]


listTransaction : Transaction -> Html Msg
listTransaction transaction =
    let
        date =
            transaction.created
                |> Date.fromTime
                |> Date.toFormattedString "d-MMM"

        ( price, transactionType ) =
            if transaction.price < 0 then
                ( "(" ++ toString -transaction.price ++ ")", Expense )
            else
                ( toString transaction.price, Earning )

        category =
            transaction.category

        description =
            transaction.description

        priceClass =
            case transactionType of
                Expense ->
                    "tr-price"

                Earning ->
                    "tr-earning"
    in
    tr []
        [ td [] [ text date ]
        , td [ class priceClass ] [ text price ]
        , td [] [ text category ]
        , td [] [ text description ]
        , td [] [ button [ class "delete is-medium", onClick (DeleteTransaction transaction.id) ] [] ]
        ]


transactionInputField : Model -> Html Msg
transactionInputField model =
    let
        ( categories, categoryTypeColor ) =
            case model.selectedCategoryType of
                Expense ->
                    ( .expenseCategories (Maybe.withDefault (dummyBook "dummy") model.currentBook)
                    , "is-dark"
                    )

                Earning ->
                    ( .earningCategories (Maybe.withDefault (dummyBook "dummy") model.currentBook)
                    , "is-link"
                    )
    in
    Html.form [ class "field", onSubmit AddTransaction, onEscape CancelTransactionInput ]
        [ div [ class "field has-addons" ]
            [ p [ class "control" ]
                [ a [ class ("button " ++ categoryTypeColor), onClick ChangeCategoryType ]
                    [ text (toString model.selectedCategoryType) ]
                ]
            , p [ class "control has-icons-left" ]
                [ input
                    [ class "input"
                    , id "tr-input-price"
                    , type_ "text"
                    , placeholder "price"
                    , maxlength 15
                    , value model.inputPrice
                    , onInput InputPrice
                    , style [ ( "width", "100px" ) ]

                    -- , attribute "min" "0"
                    -- , attribute "step" "0.01"
                    ]
                    []
                , icon "fa-money" "is-left"
                ]
            , div [ class "control" ]
                [ div [ class "select", onInput ChangeCategory ]
                    [ select []
                        (List.map (categoryToOptionSelected model.selectedCategory) categories)
                    ]
                ]
            , div [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , placeholder "description"
                    , maxlength 150
                    , value model.inputDescription
                    , onInput InputDescription

                    -- , style [ ( "width", "200px" ) ]
                    ]
                    []
                ]
            , div [ class "control" ]
                [ button [ class "button is-dark", type_ "submit" ] [ icon "fa-plus" "" ] ]
            , div [ class "control" ]
                [ button [ class "button is-danger", onClick CancelTransactionInput ] [ icon "fa-close" "" ] ]
            ]
        ]


icon : String -> String -> Html Msg
icon name additionalAttributes =
    span [ class ("icon " ++ additionalAttributes) ] [ i [ class ("fa " ++ name) ] [] ]


categoryToOptionSelected : String -> String -> Html Msg
categoryToOptionSelected selectedName name =
    if selectedName == name then
        option [ value name, selected True ] [ text name ]
    else
        categoryToOption name


categoryToOption : String -> Html Msg
categoryToOption name =
    option [ value name ] [ text name ]
