module HaddockModule where 

import Control.Monad
import Control.Monad.State
import Data.FiniteMap
import Data.IORef
import HsSyn
import HaddockTypes
import HaddockUtil
import Binary
import HaddockHtml 
import IRC
import Util
import Maybe (fromMaybe)

newtype HaddockModule = HaddockModule ()

theModule = MODULE haddockModule

haddockModule = HaddockModule ()

instance IRC.Module HaddockModule where
    moduleName   _ = return "type"
    commands     _ = return ["index"]
    moduleInit   _ = do strindex <- liftIO $ indxs ["GLUT.haddock","QuickCheck.haddock","haskell-src.haddock","network.haddock","readline.haddock","OpenGL.haddock","base.haddock","haskell98.haddock","parsec.haddock","unix.haddock"]
                        makeInitialState "haddock" strindex
    process _ _ src "index" name = 
        do maybeHaddockFMRef <- gets (\s -> lookupFM (ircModuleState s) "haddock")
           case maybeHaddockFMRef 
                of 
                Just haddockFMRef ->
                    do haddockFMState <- liftIO $ readIORef haddockFMRef
                       let (hadkState :: FiniteMap String [String]) = stripMS haddockFMState
                       ircPrivmsg src $ Util.join "," $ fromMaybe ["bzzt"] $ lookupFM hadkState name
                Nothing -> return ()

emptyState = (emptyFM :: FiniteMap String [String])

--indxs        :: [FilePath] -> IO (FiniteMap String [String])
indxs fpaths = do indices <- (mapM indx fpaths)
                  return $ foldr1 (plusFM_C (++)) indices

indx :: FilePath -> IO (FiniteMap String [String])
indx fpath = 
    do mlist <- readIface fpath
       let findx = full_index mlist
           strndx = map (\(x,y) -> (x,stringit y)) $ fmToList findx
       return $ listToFM strndx
           --where stringy fx = map sndfmToList fx
stringit   :: (FiniteMap HsQName [(HsSyn.Module, Bool)]) -> [String]
stringit x = map (show.fst) $ concatMap snd $ fmToList x

-- from Main

readIface :: FilePath -> IO [(HsSyn.Module,Interface)]
readIface filename = do
  bh <- readBinMem filename
  stuff <- Binary.get bh
  return (map to_interface stuff)
 where 
   to_interface (mdl, package, hide, env, reexported, sub) = 
	  (mdl, Interface { 
		   iface_filename     = "",
		   iface_package      = package,
		   iface_env          = listToFM env,
		   iface_import_env   = emptyFM,
		   iface_sub	      = listToFM sub,
		   iface_reexported   = listToFM reexported,
		   iface_exports      = [],
		   iface_orig_exports = [],
		   iface_insts	      = [],
		   iface_decls        = emptyFM,
		   iface_info	      = Nothing,
		   iface_doc          = Nothing,
		   iface_options      = if hide then [OptHide] else []
		}
      	  )

-- from HaddockHtml
