{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main (main) where
import Assort(fmapTest)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

main :: IO ()
main = do
    t <- runQ [t|forall self. Int -> self|]
    fun1 <- runQ $ fmapTest t
    putStr "fmap is: "
    case fun1 of
        Nothing -> putStrLn "not found"
        Just Nothing -> putStrLn "identity"
        Just (Just f1) -> putStrLn $ pprint f1
    putStrLn "Test suite not yet implemented."
