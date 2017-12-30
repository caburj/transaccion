port module Main exposing (..)

-- elm-package install --yes elm-community/json-extra
-- import List.Extra

import Dict exposing (Dict)
import Hashids exposing (hashidsMinimum)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Json_ exposing (decodeBooks, encodeBook, encodeBooks)
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


port setStorage : String -> Cmd msg


port deleteBook : Id -> Cmd msg


port saveBook : String -> Cmd msg



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


init : Maybe String -> Nav.Location -> ( Model, Cmd Msg )
init maybeBooks loc =
    case maybeBooks of
        Nothing ->
            Model HomeR initBooks "" "" "" Nothing 0 ! [ getTimeNow ]

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
            Model HomeR books "" "" "" Nothing 0 ! [ getTimeNow ]


initBooks : Dict String Book
initBooks =
    Dict.empty


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map HomeR Url.top
        , Url.map AppR (Url.s "app")
        , Url.map BookR (Url.s "app" </> Url.string)
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
    | InputCategory TransactionCategory String
    | AddCategory TransactionCategory
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
            { model | currentBook = Dict.get bookId model.books } ! []

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
            { newModel | books = newBooks, currentBook = Just editedBook } ! [ saveBook (encode 2 (encodeBook editedBook)), cmds ]

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
            { newModel | books = newBooks, currentBook = Just editedBook } ! [ saveBook (encode 2 (encodeBook editedBook)), cmds ]

        InputCategory category name ->
            case category of
                Expense ->
                    { model | inputExpenseCategory = name } ! []

                Earning ->
                    { model | inputEarningCategory = name } ! []

        AddCategory category ->
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
                    case category of
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
                                    ! [ saveBook (encode 2 (encodeBook newBook)), cmds ]

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
                                    ! [ saveBook (encode 2 (encodeBook newBook)), cmds ]

        _ ->
            model ! []



---- UTILITY FUNCTIONS ----


deleteCategory : TransactionCategory -> String -> Time -> Maybe Book -> Maybe Book
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


salt : Hashids.Context
salt =
    hashidsMinimum "ako ay may lobo" 5


replace : comparable -> v -> Dict comparable v -> Dict comparable v
replace key newValue dict =
    Dict.remove key dict |> Dict.insert key newValue



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ navbar
        , div [ class "app-content" ]
            [ render model ]

        -- LOG MODEL --
        -- , div []
        --     ([ let
        --         val =
        --             model.currentTime
        --        in
        --        text (toString val)
        --      ]
        --         ++ [ div [] [] ]
        --         ++ [ let
        --                 book =
        --                     Maybe.withDefault (dummyBook "") model.currentBook
        --              in
        --              text book.id
        --            ]
        --     )
        ]


navbar : Html Msg
navbar =
    nav [ class "navbar is-dark" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", onClick (NewUrl "/") ]
                [ h1 [ class "title is-light is-3" ] [ text "transaccion" ] ]
            , a [ class "navbar-item", onClick (NewUrl "/app") ]
                [ h1 [ class "subtitle is-4 is-light" ] [ text "use app" ] ]
            ]
        ]


render : Model -> Html Msg
render model =
    case model.currentRoute of
        HomeR ->
            div [ class "block" ]
                [ h1 [ class "subtitle is-4" ] [ text "This is the home page." ] ]

        AppR ->
            div [ class "columns is-multiline" ]
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
                    div [ class "block" ]
                        [ div [ class "columns" ]
                            [ div [ class "column is-two-thirds" ]
                                [ h1 [ class "title is-3" ] [ text book.name ]
                                , h1 [ class "subtitle is-6" ] [ text ("/book" ++ toString id ++ "/") ]
                                ]
                            , div [ class "column is-one-third" ]
                                [ categoriesBox book Expense DeleteExpenseCategory
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


categoriesBox : Book -> TransactionCategory -> (Id -> String -> Msg) -> Html Msg
categoriesBox book category msg =
    let
        ( color, categories, name ) =
            if category == Expense then
                ( "is-danger", book.expenseCategories, "Expense Categories" )
            else
                ( "is-success", book.earningCategories, "Earning Categories" )
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


addCategoryForm : Book -> TransactionCategory -> Html Msg
addCategoryForm book category =
    span [ style [ ( "font-size", "1rem" ), ( "font-weight", "normal" ), ( "width", "100px" ) ] ]
        [ div [ class "field has-addons" ]
            [ div [ class "control" ]
                [ input
                    [ class "input is-small"
                    , type_ "text"
                    , placeholder "new"
                    , onInput (InputCategory category)
                    ]
                    []
                ]
            , div [ class "control" ]
                [ button [ class "button is-small is-primary", onClick (AddCategory category) ]
                    [ span [ class "icon" ] [ i [ class "fa fa-plus" ] [] ] ]
                ]
            ]
        ]


bookCard : Book -> Html Msg
bookCard book =
    div [ class "column is-3" ]
        [ article [ class "message" ]
            [ div [ class "message-header" ]
                [ text ("book " ++ toString book.id)
                , button [ class "delete", onClick (DeleteBook book.id) ] []
                ]
            , div [ onClick (SelectBook book.id) ]
                [ div [ class "message-body", onClick (NewUrl ("/app/" ++ book.id)) ] [ text book.name ] ]
            ]
        ]


addBookForm : Model -> Html Msg
addBookForm model =
    div [ class "column is-3" ]
        [ div [ class "field has-addons" ]
            [ p [ class "control" ]
                [ input [ class "input", type_ "text", placeholder "name", onInput InputBookName ] [] ]
            , p [ class "control" ]
                [ button [ class "button is-dark", onClick (AddBook model.inputBookName) ]
                    [ span [ class "icon" ]
                        [ i [ class "fa fa-plus" ] [] ]
                    ]
                ]
            ]
        ]
