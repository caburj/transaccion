port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as ListE
import Navigation as Nav
import UrlParser as Url exposing ((</>))


---- PROGRAM ----


main : Program (Maybe (List Book)) Model Msg
main =
    Nav.programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- PORTS ----


port setStorage : List Book -> Cmd msg



---- MODEL ----


type alias Model =
    { currentRoute : Route
    , books : List Book
    , inputBookName : String
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
    , transactions : List Transaction
    }


type alias Transaction =
    { id : Id
    , price : Float
    , category : String
    , description : String
    }


type alias Id =
    Int


defaultExpenseCategories : List String
defaultExpenseCategories =
    [ "Uncategorized", "Food", "Rent", "Transpo", "Leisure", "Misc", "Subscription", "Medical", "Unexpected" ]


defaultEarningCategories : List String
defaultEarningCategories =
    [ "Uncategorized", "Salary", "Bonus", "Gift", "Reimbursement", "Sideline", "Unexpected" ]


defaultBook1 : Book
defaultBook1 =
    Book 0 "Personal" defaultExpenseCategories defaultEarningCategories []


defaultBook2 : Book
defaultBook2 =
    Book 1 "Business" defaultExpenseCategories defaultEarningCategories []


init : Maybe (List Book) -> Nav.Location -> ( Model, Cmd Msg )
init maybeBooks loc =
    case maybeBooks of
        Nothing ->
            Model HomeR initBooks "" ! []

        Just books ->
            Model HomeR books "" ! []


initBooks : List Book
initBooks =
    [ defaultBook1, defaultBook2 ]


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map HomeR Url.top
        , Url.map AppR (Url.s "app")
        , Url.map BookR (Url.s "app" </> Url.int)
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
                            Book id name defaultExpenseCategories defaultEarningCategories []

                        newBooks =
                            model.books ++ [ newBook ]
                    in
                    { model | books = newBooks } ! [ setStorage newBooks ]

        DeleteBook bookId ->
            let
                newBooks =
                    List.filter (\book -> book.id /= bookId) model.books
            in
            { model | books = newBooks } ! [ setStorage newBooks ]

        DeleteExpenseCategory bookId name ->
            let
                maybeBook =
                    ListE.find (\b -> b.id == bookId) model.books

                newBooks =
                    case maybeBook of
                        Nothing ->
                            model.books

                        Just book ->
                            let
                                newCategories =
                                    List.filter (\n -> n /= name) book.expenseCategories

                                newBook =
                                    { book | expenseCategories = newCategories }

                                unsortedNewBooks =
                                    newBook :: List.filter (\b -> b.id /= bookId) model.books
                            in
                            List.sortBy .id unsortedNewBooks
            in
            { model | books = newBooks } ! [ setStorage newBooks ]

        DeleteEarningCategory bookId name ->
            let
                maybeBook =
                    ListE.find (\b -> b.id == bookId) model.books

                newBooks =
                    case maybeBook of
                        Nothing ->
                            model.books

                        Just book ->
                            let
                                newCategories =
                                    List.filter (\n -> n /= name) book.earningCategories

                                newBook =
                                    { book | earningCategories = newCategories }

                                unsortedNewBooks =
                                    newBook :: List.filter (\b -> b.id /= bookId) model.books
                            in
                            List.sortBy .id unsortedNewBooks
            in
            { model | books = newBooks } ! [ setStorage newBooks ]

        NoOp ->
            model ! []



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
                (List.map bookCard model.books
                    ++ [ addBookForm model ]
                )

        BookR id ->
            let
                maybeBook =
                    ListE.find (\book -> book.id == id) model.books
            in
            case maybeBook of
                Just book ->
                    div [ class "block" ]
                        [ h1 [ class "title is-3" ] [ text book.name ]
                        , h1 [ class "subtitle is-6" ] [ text (toString id) ]
                        , deletableTags "is-danger" (DeleteExpenseCategory book.id) book.expenseCategories
                        , deletableTags "is-success" (DeleteEarningCategory book.id) book.earningCategories
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


bookCard : Book -> Html Msg
bookCard book =
    div [ class "column is-3" ]
        [ article [ class "message" ]
            [ div [ class "message-header" ]
                [ text ("book " ++ toString book.id)
                , button [ class "delete", onClick (DeleteBook book.id) ] []
                ]
            , div [ class "message-body", onClick (NewUrl ("/app/" ++ toString book.id)) ] [ text book.name ]
            ]
        ]


addBookForm : Model -> Html Msg
addBookForm model =
    div [ class "column is-3" ]
        [ div [ class "field has-addons" ]
            [ p [ class "control" ]
                [ input [ class "input", type_ "text", placeholder "name", onInput InputBookName ] [] ]
            , p [ class "control" ]
                [ button [ class "button is-dark", onClick (AddBook (List.length model.books) model.inputBookName) ]
                    [ span [ class "icon" ]
                        [ i [ class "fa fa-plus" ] [] ]
                    ]
                ]
            ]
        ]
