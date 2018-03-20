module DateFormatting exposing (viewTime)

import Date exposing (..)
import Time exposing (Time)
import Time.Format exposing (format)


norwegianLabelForDay : Day -> String
norwegianLabelForDay day =
    case day of
        Mon ->
            "mandag"

        Tue ->
            "tirsdag"

        Wed ->
            "onsdag"

        Thu ->
            "torsdag"

        Fri ->
            "fredag"

        Sat ->
            "lørdag"

        Sun ->
            "søndag"


norwegianLabelForMonth : Month -> String
norwegianLabelForMonth month =
    case month of
        Jan ->
            "januar"

        Feb ->
            "februar"

        Mar ->
            "mars"

        Apr ->
            "april"

        May ->
            "mai"

        Jun ->
            "juni"

        Jul ->
            "juli"

        Aug ->
            "august"

        Sep ->
            "september"

        Oct ->
            "oktober"

        Nov ->
            "november"

        Dec ->
            "desember"


viewTime : Time -> String
viewTime time =
    let
        clock =
            format "%H:%M" time

        asDate =
            Date.fromTime time

        date =
            (norwegianLabelForDay <| dayOfWeek asDate)
                ++ " "
                ++ (toString <| day asDate)
                ++ ". "
                ++ (norwegianLabelForMonth <| month asDate)
    in
    date
        ++ " "
        ++ clock
