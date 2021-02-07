module GeneralForutsetninger exposing (..)


verdisettinger =
    -- alle verdier i NOK
    { reisetidSykkel =
        -- VoT_Sykkel
        -- pr min pr syklist
        147.47 / 60
    , tsKostnadSykkel = 3.50891113
    , tsGevinstLEDLysSyklende = 1 -- 100 %
    , reisetidKollektivTransport =
        -- pr min pr passasjer
        1.29
    , reisetidBil =
        -- pr min pr bil
        2.46
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
