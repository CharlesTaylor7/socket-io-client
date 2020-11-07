import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.InstallDirs (absoluteInstallDirs)
import Distribution.Simple.Setup

import Distribution.Simple.Utils (installOrdinaryFiles, rawSystemExit)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postBuild = installNodeDeps }


installNodeDeps :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installNodeDeps args flags desc info = do
  -- let installDirs = absoluteInstallDirs _ _ _
  -- dataDir = datadir installDirs
  -- rawSystemExit "cd data_dir; yarn"
  pure ()

