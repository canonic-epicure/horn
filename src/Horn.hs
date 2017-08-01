module Horn(
    someFunc
) where

someFunc = undefined

--import              Data.Text
--
--import              Data.Int
--
--import qualified    Data.Functor.Contravariant as DFC
--
--import qualified    Hasql.Connection as HC
--import qualified    Hasql.Session as HS
--import qualified    Hasql.Query as HQ
--import qualified    Hasql.Encoders as HE
--import qualified    Hasql.Decoders as HD
--
--------------------------------------------------------------------------------------------------------------------------
--someFunc :: IO ()
--
--selectSumQuery :: HQ.Query (Int64, Int64) [ Text ]
--
--selectSumQuery =
--    HQ.statement sql encoder decoder True
--    where
--        sql         = "SELECT table_name FROM information_schema.tables as t where t.table_schema = 'public'"
--
--        encoder     = DFC.contramap fst (HE.value HE.int8) <> DFC.contramap snd (HE.value HE.int8)
--        decoder     = HD.rowsList (HD.value HD.text)
--
--selectSumSession = HS.query (1, 2) selectSumQuery
--
--someFunc = do
--    putStrLn "Connecting to postgres"
--
--    res <- HC.acquire settings
--
--    case res of
--        Left connectionError ->
--            putStrLn "Connection error"
--        Right connection -> do
--            queryRes <- HS.run selectSumSession connection
--            case queryRes of
--                Left error ->
--                    putStrLn "Query error"
--                Right res -> do
--                    putStrLn (show res)
--
--
--
--settings = HC.settings "localhost" 5432 "postgres" "123" "postgres"
--
--
