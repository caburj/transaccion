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



-- PORTS --


port setStorage : List Book -> Cmd msg



---- MODEL ----


type alias Model =
    { currentRoute : Route
    , books : List Book
    , inputBookName : String
    }


type alias Book =
    { id : Int
    , name : String
    , transactions : List Transaction
    }


type alias Transaction =
    { id : Int
    , price : Float
    , category : String
    , description : String
    }


type Route
    = Home
    | App
    | BookR Int
    | PageNotFound


init : Maybe (List Book) -> Nav.Location -> ( Model, Cmd Msg )
init maybeBooks loc =
    case maybeBooks of
        Nothing ->
            Model Home initBooks "" ! []

        Just books ->
            Model Home books "" ! []


initBooks : List Book
initBooks =
    [ Book 0 "Personal" [], Book 1 "Business" [] ]


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map App (Url.s "app")
        , Url.map BookR (Url.s "app" </> Url.int)
        ]



---- UPDATE ----


type Msg
    = UrlChange Nav.Location
    | NewUrl String
    | InputBookName String
    | AddBook BookId String
    | Delete BookId


type alias BookId =
    Int


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
                            PageNotFound

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
                            Book id name []

                        newBooks =
                            model.books ++ [ newBook ]
                    in
                    { model | books = newBooks } ! [ setStorage newBooks ]

        Delete bookId ->
            let
                newBooks =
                    List.filter (\book -> book.id /= bookId) model.books
            in
            { model | books = newBooks } ! [ setStorage newBooks ]



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
        Home ->
            div [ class "block" ]
                [ h1 [ class "subtitle is-4" ] [ text "This is the home page." ] ]

        App ->
            div [ class "columns is-multiline" ]
                (List.map showBookCard model.books
                    ++ [ addBookField model ]
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
                        , h1 [ class "subtitle is-4" ] [ text (toString id) ]
                        ]

                Nothing ->
                    div [ class "block" ]
                        [ text ("There is no book with that id: " ++ toString id) ]

        PageNotFound ->
            div [] [ text "The page is not available." ]


showBookCard : Book -> Html Msg
showBookCard book =
    div [ class "column is-3" ]
        [ article [ class "message" ]
            [ div [ class "message-header" ]
                [ text ("book " ++ toString book.id)
                , button [ class "delete", onClick (Delete book.id) ] []
                ]
            , div [ class "message-body", onClick (NewUrl ("/app/" ++ toString book.id)) ] [ text book.name ]
            ]
        ]


addBookField : Model -> Html Msg
addBookField model =
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
