module Helper exposing (..)

import Date exposing (Date, Month(..))
import Round


-- import Date.Extra as Date


monthFromString : String -> Month
monthFromString monthString =
    case monthString of
        "Jan" ->
            Jan

        "Feb" ->
            Feb

        "Mar" ->
            Mar

        "Apr" ->
            Apr

        "May" ->
            May

        "Jun" ->
            Jun

        "Jul" ->
            Jul

        "Aug" ->
            Aug

        "Sep" ->
            Sep

        "Oct" ->
            Oct

        "Nov" ->
            Nov

        _ ->
            Dec


toTwoDecimal : Float -> String
toTwoDecimal =
    Round.round 2
