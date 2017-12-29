module Json_ exposing (..)

-- elm-package install --yes NoRedInk/elm-decode-pipeline

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Model exposing (..)


decodeTransaction : Decode.Decoder Transaction
decodeTransaction =
    Pipeline.decode Transaction
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "price" Decode.float
        |> Pipeline.required "category" Decode.string
        |> Pipeline.required "description" Decode.string


encodeTransaction : Transaction -> Encode.Value
encodeTransaction record =
    Encode.object
        [ ( "id", Encode.string <| record.id )
        , ( "price", Encode.float <| record.price )
        , ( "category", Encode.string <| record.category )
        , ( "description", Encode.string <| record.description )
        ]


decodeBook : Decode.Decoder Book
decodeBook =
    Pipeline.decode Book
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "expenseCategories" (Decode.list Decode.string)
        |> Pipeline.required "earningCategories" (Decode.list Decode.string)
        |> Pipeline.required "transactions" (Decode.dict decodeTransaction)


encodeBook : Book -> Encode.Value
encodeBook record =
    Encode.object
        [ ( "id", Encode.string <| record.id )
        , ( "name", Encode.string <| record.name )
        , ( "expenseCategories", Encode.list <| List.map Encode.string <| record.expenseCategories )
        , ( "earningCategories", Encode.list <| List.map Encode.string <| record.earningCategories )
        , ( "transactions", EncodeExtra.dict identity encodeTransaction record.transactions )
        ]


decodeBooks : Decode.Decoder (Dict Id Book)
decodeBooks =
    Decode.dict decodeBook


encodeBooks : Dict Id Book -> Encode.Value
encodeBooks books =
    EncodeExtra.dict identity encodeBook books
