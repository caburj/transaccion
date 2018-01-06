module Footer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main =
    node "footer"
        [ id "footer", class "footer" ]
        [ div []
            [ div []
                [ p []
                    [ strong [] [ text "transaccion" ]
                    , text " by "
                    , a [ href "https://github.com/caburj" ] [ text "Joseph Caburnay" ]
                    , text " and Jean Katrine Boyles."
                    ]
                , p []
                    [ text "Cooked in "
                    , a [ href "https://elm-lang.org/" ] [ text "Elm" ]
                    , text ". Flavored with "
                    , a [ href "https://bulma.io" ] [ text "Bulma" ]
                    , text " and "
                    , a [ href "https://remotestorage.io" ] [ text "remotestoragejs." ]
                    ]
                , p []
                    [ text "Licensed under the "
                    , a [ href "https://www.apache.org/licenses/LICENSE-2.0" ] [ text "Apache License 2.0." ]
                    ]
                ]
            ]
        ]
