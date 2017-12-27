module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation as Nav
import UrlParser as Url exposing ((</>))


---- MODEL ----


type alias Model =
    { currentRoute : Route
    , books : List Book
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


init : Nav.Location -> ( Model, Cmd Msg )
init loc =
    ( Model Home initBooks, Cmd.none )


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



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ navbar
        , render model
        ]


navbar : Html Msg
navbar =
    nav [ class "navbar is-dark" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item is-light", onClick (NewUrl "/") ]
                [ h1 [ class "title is-light is-3" ] [ text "transaccion" ] ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ a [ class "navbar-item", onClick (NewUrl "/app") ] [ h1 [ class "subtitle is-4 is-light" ] [ text "use app" ] ] ]
            ]
        ]


render : Model -> Html Msg
render model =
    case model.currentRoute of
        Home ->
            div [ class "block" ]
                [ h1 [ class "subtitle is-4" ] [ text "This is the home page." ] ]

        App ->
            div [ class "columns" ] (List.map showBook model.books)

        BookR id ->
            div [ class "block" ] [ text ("book " ++ toString id) ]

        PageNotFound ->
            div [] [ text "The page is not available." ]


showBook : Book -> Html Msg
showBook book =
    div [ class "column is-3", onClick (NewUrl ("/app/" ++ toString book.id)) ]
        [ article [ class "message" ]
            [ div [ class "message-header" ]
                [ text ("book " ++ toString book.id)
                , button [ class "delete" ] []
                ]
            , div [ class "message-body" ] [ text book.name ]
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Nav.program UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
