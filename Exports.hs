{-# LANGUAGE CPP #-}
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
import GhcMonad
import GHC.Paths
import Outputable
import System.FilePath.Posix
import Prelude hiding (concat, mod)

autoModule :: FilePath -> IO ()
autoModule mod = runGhc (Just GHC.Paths.libdir) $ do
  dflags <- GHC.getSessionDynFlags
#if (__GLASGOW_HASKELL__ > 706)
  _ <- setSessionDynFlags ( dflags `gopt_set` Opt_GhciSandbox )
#else
  _ <- setSessionDynFlags ( dflags `dopt_set` Opt_GhciSandbox )
#endif
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
  GhcMonad.liftIO (mapM_ (showThing dflags) sortedThings)

showNamed :: NamedThing a => DynFlags -> a -> IO ()
showNamed df a = do
  let nm = showSDoc df (ppr (getOccName a))
  let mod = showSDoc df (ppr (nameModule (getName a)))
  putStrLn (nm ++ " from " ++ mod)

showThing :: DynFlags -> TyThing -> IO ()
showThing df (AnId a) = showNamed df a
#if (__GLASGOW_HASKELL__ > 706)
showThing df (AConLike a) = showNamed df a
#endif
showThing df (ATyCon a) = showNamed df a
showThing _ _ = error "Should never happen."

--showGhc :: (Outputable a) => a -> String
--showGhc = showPpr unsafeGlobalDynFlags

main :: IO ()
main = autoModule "Protolude"
