port module Main exposing (..)

-- elm-package install --yes elm-community/json-extra
-- import List.Extra

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Json_ exposing (decodeBooks, encodeBook, encodeBooks)
import Model exposing (..)
import Navigation as Nav
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
    Book ("dummy" ++ id) ("dummy" ++ id) [] [] Dict.empty


init : Maybe String -> Nav.Location -> ( Model, Cmd Msg )
init maybeBooks loc =
    case maybeBooks of
        Nothing ->
            Model HomeR initBooks "" "" "" Nothing ! []

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
            Model HomeR books "" "" "" Nothing ! []


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
    | NewUrl String
    | InputBookName String
    | AddBook Id String
    | DeleteBook Id
    | DeleteExpenseCategory Id String
    | DeleteEarningCategory Id String
    | InputCategory Book TransactionCategory String
    | AddCategory Book TransactionCategory
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            { model | currentRoute = cRoute } ! []

        InputBookName bookName ->
            { model | inputBookName = bookName } ! []

        AddBook id name ->
            case name of
                "" ->
                    model ! []

                _ ->
                    let
                        newBook =
                            Book id name defaultExpenseCategories defaultEarningCategories Dict.empty

                        newBooks =
                            Dict.insert id newBook model.books
                    in
                    { model | books = newBooks } ! [ saveBook (encode 2 (encodeBook newBook)) ]

        DeleteBook bookId ->
            let
                newBooks =
                    Dict.remove bookId model.books
            in
            { model | books = newBooks } ! [ deleteBook bookId ]

        DeleteExpenseCategory bookId name ->
            let
                newBooks =
                    Dict.update bookId (deleteCategory Expense name) model.books

                editedBook =
                    Maybe.withDefault (dummyBook (toString <| Dict.size model.books)) (Dict.get bookId newBooks)
            in
            { model | books = newBooks } ! [ saveBook (encode 2 (encodeBook editedBook)) ]

        DeleteEarningCategory bookId name ->
            let
                newBooks =
                    Dict.update bookId (deleteCategory Earning name) model.books

                editedBook =
                    Maybe.withDefault (dummyBook (toString <| Dict.size model.books)) (Dict.get bookId newBooks)
            in
            { model | books = newBooks } ! [ saveBook (encode 2 (encodeBook editedBook)) ]

        InputCategory book category name ->
            model ! []

        AddCategory category book ->
            model ! []

        NoOp ->
            model ! []



---- UTILITY FUNCTIONS ----


deleteCategory : TransactionCategory -> String -> Maybe Book -> Maybe Book
deleteCategory category name maybeBook =
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
                    Just { book | expenseCategories = newCategories }

                Earning ->
                    let
                        newCategories =
                            List.filter (\n -> n /= name) book.earningCategories
                    in
                    Just { book | earningCategories = newCategories }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ navbar
        , div [ class "app-content" ]
            [ render model ]
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
                (List.map bookCard (Dict.values model.books)
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


deletableTags : String -> (String -> Msg) -> List String -> Html Msg
deletableTags color xMessage names =
    div [ class "field is-grouped is-grouped-multiline" ]
        (names
            |> List.map (xTag color xMessage)
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
                        [ div [ class "column is-two-thirds" ] [ p [ class "subtitle is-5" ] [ text name ] ]
                        , div [ class "column" ] [ addCategoryForm book category ]
                        ]
                    , hr [] []
                    , deletableTags color (msg book.id) categories
                    ]
                ]
            ]
        ]


addCategoryForm : Book -> TransactionCategory -> Html Msg
addCategoryForm book category =
    span [ style [ ( "font-size", "1rem" ), ( "font-weight", "normal" ) ] ]
        [ div [ class "field has-addons" ]
            [ div [ class "control" ]
                [ input [ class "input is-small", type_ "text", onInput (InputCategory book category) ] [] ]
            , div [ class "control" ]
                [ button [ class "button is-small is-primary", onClick (AddCategory book category) ]
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
            , div [ class "message-body", onClick (NewUrl ("/app/" ++ book.id)) ] [ text book.name ]
            ]
        ]


addBookForm : Model -> Html Msg
addBookForm model =
    div [ class "column is-3" ]
        [ div [ class "field has-addons" ]
            [ p [ class "control" ]
                [ input [ class "input", type_ "text", placeholder "name", onInput InputBookName ] [] ]
            , p [ class "control" ]
                [ button [ class "button is-dark", onClick (AddBook (toString (Dict.size model.books)) model.inputBookName) ]
                    [ span [ class "icon" ]
                        [ i [ class "fa fa-plus" ] [] ]
                    ]
                ]
            ]
        ]
