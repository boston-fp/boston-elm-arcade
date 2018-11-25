module Games.Chansey.EggType exposing (..)

type EggType
    = EggTypeEgg
    | EggTypeBomb


score : EggType -> Int
score typ =
    case typ of
        EggTypeEgg ->
            1

        EggTypeBomb ->
            -5
