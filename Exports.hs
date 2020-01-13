module Main
  ( main,
  )
where

import Control.Applicative ((<$>))
import Control.Monad.Trans
import Data.Foldable (concat)
import qualified Data.List as List
import Data.Maybe
import Data.Ord (comparing)
import DynFlags
import GHC
import GHC.Paths
import Outputable
import System.FilePath.Posix
import Prelude hiding (concat, mod)

autoModule :: FilePath -> IO ()
autoModule mod = runGhc (Just GHC.Paths.libdir) $ do
  dflags <- GHC.getSessionDynFlags
  _ <-
    setSessionDynFlags
      ( dflags `gopt_set` Opt_KeepRawTokenStream
          `gopt_set` Opt_Haddock
          `gopt_set` Opt_GhciSandbox
      )
  target <- guessTarget ("src" </> addExtension mod ".hs") Nothing
  setTargets [target]
  _ <- load LoadAllTargets
  modSum <- getModSummary $ mkModuleName mod
  p <- GHC.parseModule modSum
  t <- typecheckModule p
  let modInfo = tm_checked_module_info t
  let exports = modInfoExports modInfo
  exportThings <- sequence <$> mapM lookupName exports
  let sortedThings = List.sortBy (comparing getOccName) (concat exportThings)
  liftIO $ mapM_ (showThing) sortedThings

showNamed :: NamedThing a => a -> IO ()
showNamed a = do
  let nm = showGhc (getOccName a)
  let mod = showGhc (nameModule (getName a))
  putStrLn (nm ++ " from " ++ mod)

showThing :: TyThing -> IO ()
showThing (AnId a) = showNamed a
showThing (AConLike a) = showNamed a
showThing (ATyCon a) = showNamed a
showThing _ = error "Should never happen."

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

main :: IO ()
main = autoModule "Protolude"
