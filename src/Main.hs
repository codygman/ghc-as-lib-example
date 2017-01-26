import GHC
import Outputable
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths

import DynFlags
targetFile = "B.hs"

main :: IO ()
main = do
   res <- example
   str <- runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      return $ showSDoc dflags $ ppr res
   putStrLn str

example =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        -- let dflags' = foldl xopt_set dflags [Opt_ImplicitPrelude]
        let dflags' = foldl xopt_set dflags []
        setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "B"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d

        g <- getModuleGraph
        mapM showModule g
        return $ (parsedSource d,"/n-----/n",  typecheckedSource d)
