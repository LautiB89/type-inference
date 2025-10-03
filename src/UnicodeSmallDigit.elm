module UnicodeSmallDigit exposing (shrinkDigits)


fromDigit : Char -> Char
fromDigit c =
    case c of
        '1' ->
            '₁'

        '2' ->
            '₂'

        '3' ->
            '₃'

        '4' ->
            '₄'

        '5' ->
            '₅'

        '6' ->
            '₆'

        '7' ->
            '₇'

        '8' ->
            '₈'

        '9' ->
            '₉'

        '0' ->
            '₀'

        _ ->
            c


shrinkDigits : String -> String
shrinkDigits =
    String.map fromDigit
