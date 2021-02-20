module GeneralForutsetninger exposing (..)


verdisettinger =
    -- alle verdier i NOK
    { reisetidSykkel =
        -- VoT_Sykkel
        -- pr min pr syklist
        147.47 / 60
    , tsKostnadSykkel = 3.50891113
    , tsKostnadBil = 0.2639315
    , tsKostnadKollektiv = 0.0125062
    , tsKostnadGange = 5.2089111
    , tsGevinstLEDLysSyklende = 100 / 100
    , sykkelBedreBelysningLED = 5 / 100
    , fotgjengerBedreBelysningLED = 5 / 100
    , andelNyeSyklisterFraBil = 40 / 100
    , andelNyeSyklisterFraKollektivtransport = 35 / 100
    , andelNyeSyklisterFraGange = 0
    , andelNyeSyklisterGenererte = 25 / 100
    , andelNyeFotgjengereFraBil = 40 / 100
    , koekostnadBiler =
        -- kr pr km per bil
        2.0890356
    , eksterneKostnaderBil =
        -- kr pr P-km
        0.4428013
    , eksterneKostnaderSykkel =
        -- kr pr P-km
        0
    , eksterneKostnaderKollektiv =
        -- kr pr P-km
        0.2122847
    , helseTSGevinstSykkel =
        -- kr pr km syklet
        22.8
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
