module GeneralForutsetninger exposing (..)


verdisettinger =
    { voTGange =
        -- VoT_Gange
        -- kr pr. min pr. gangtur
        2.88
    , voTSykkel =
        -- VoT_Sykkel
        -- kr. pr min pr syklist
        1.93
    , helseGevinstGange =
        -- kr pr km gange
        23.43
    , helseGevinstSykkel =
        -- kr pr km syklet
        14.78
    , tsKostnadGange = 0.984932862
    , tsKostnadSykkel = 1.386689163
    , syklistTotalReiseDistanceKm = 5
    , fotgjengerTotalReiseDistanceKm = 2
    }


drente : Float
drente =
    4 / 100


vekstrate : Float
vekstrate =
    1.3 / 100


drenteVekst : Float
drenteVekst =
    (drente - vekstrate) / (1 + vekstrate)


analysePeriode : number
analysePeriode =
    40


investeringsFaktor : Float -> Float
investeringsFaktor levetid =
    -- denne funksjonen virker relatert til afaktorCalculation
    let
        beregningsTekniskMellomregning =
            toFloat <| (analysePeriode // truncate levetid) + 1

        ledd1 =
            (1 - ((1 + drente) ^ (negate levetid * beregningsTekniskMellomregning)))
                / (1 - ((1 + drente) ^ negate levetid))

        ledd2 =
            (analysePeriode - (levetid * beregningsTekniskMellomregning))
                / (levetid * ((1 + drente) ^ analysePeriode))
    in
    ledd1 + ledd2


afaktorCalculation : Float -> Float
afaktorCalculation drenteValue =
    (1 / drenteValue) * (1 - (1 / ((1 + drenteValue) ^ analysePeriode)))


afaktorVekst : Float
afaktorVekst =
    afaktorCalculation drenteVekst


afaktor : Float
afaktor =
    afaktorCalculation drente


skyggepris : Float
skyggepris =
    0.2
