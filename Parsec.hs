import Text.Parsec
import Text.Parsec.Text

------------------------------------------------------------------------------
-- Generate Parsec Parser via TH
------------------------------------------------------------------------------

newtype TH a = TH { runTH :: ExpQ }

instance ApplicativeRepr TH where
    pureR a = TH [| a |]
    app (TH f) (TH x) = TH (appE (appE (varE '(<*>)) f) x)
    appL (TH f) (TH x) = TH (appE (appE (varE '(<*)) f) x)
    appR (TH f) (TH x) = TH (appE (appE (varE '(*>)) f) x)

instance Pair TH where
    pair = TH [| pure (,) |]
    prj1 = TH [| pure fst |]
    prj2 = TH [| pure snd |]

instance ABNFParser TH where
    pCharVal t = TH [| fmap pack $ string (unpack t) |]
    pHexChar s =
        let c = chr (read $ "0x" ++ s)
        in TH [| char c |]
--    pAnyChar   = TH [| anyChar |]
    pEnumerate choices = TH [| choice (map (\(t, v) -> string (unpack t) >> return v) choices) |]
