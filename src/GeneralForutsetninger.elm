module GeneralForutsetninger exposing (..)

-- TODO: grupper verdisettinger som varierer etter hva de varierer på
-- TODO: Datatype Nivaa: lav-middels, lav-høy, middels-høy
-- Noen varierer på nivå: lav-middels, lav-høy, middels-høy
-- Noen varierer på sted spredtbygd, liten by, stor by
-- Vi kan begynne med å introdusere nivåer ved å postulere at dagens program er lav til høy
-- Vi kunne uttrykt nivå bestemte verdier ved å lagre 2 tall (sjekk med nils). Enten lav til høy og lav til middels (så kan man regne ut differansen for å finne ut middels til høy).


verifiserteVerdisettinger =
    { voTGange =
        -- VoT_Gange
        -- kr pr. min pr. gangtur
        2.77293673883309
    , voTSykkel =
        -- VoT_Sykkel
        -- kr. pr min pr syklist
        147.47 / 60
    , helseGevinstGange =
        -- kr pr km gange
        61.3491315
    , tsKostnadGange = 1.8333
    }


verdisettinger =
    -- alle verdier i NOK
    { tsKostnadSykkel = 3.50891113
    , tsKostnadBil = 0.2639315
    , tsKostnadKollektiv = 0.0125062

    -- skadereduksjon %
    , tsGevinstLEDLysSyklende = 100 / 100
    , tsGevinstLEDLysGaaende = 0 / 100
    , tsGevinstGsB_GsASyklende = 1 / 100
    , tsGevinstGsB_GsAGaaende = 20 / 100

    -- Etterspørselseffekter
    , sykkelBedreBelysningLED = 5 / 100
    , fotgjengerBedreBelysningLED = 5 / 100
    , sykkelGsB_GsA = 5 / 100
    , fotgjengerGsB_GsA = 5 / 100

    -- Overføring
    , andelNyeSyklisterFraBil = 40 / 100
    , andelNyeFotgjengereFraBil = 40 / 100
    , andelNyeSyklisterFraKollektivtransport = 35 / 100
    , andelNyeFotgjengereFraKollektivtransport = 35 / 100
    , andelNyeSyklisterGenererte = 25 / 100
    , andelNyeFotgjengereGenererte = 25 / 100

    -- Andre verdier
    , koekostnadBiler =
        -- kr pr km per bil
        2.0890356
    , eksterneKostnaderSykkel =
        -- kr pr P-km
        0
    , eksterneKostnaderGange =
        -- kr pr P-km
        0
    , helseTSGevinstSykkel =
        -- kr pr km syklet
        22.8
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
