module Main where
import System.Directory -- listDirectory, doesFIleExist, doesDirectoryExist
import System.FilePath.Posix -- </>, takeExtension, takeFileName
import Control.Monad -- guard, liftM2
import System.Environment -- getArgs
import Data.List -- intercalate
import Control.Applicative -- <|>
import Control.Monad.Trans.Maybe -- MaybeT
import Control.Monad.IO.Class -- liftIO

path :: String
path = "/Users/casperthule/source/overture_project/overture/core/codegen/isagen/src/test/resources/exps/"

main :: IO ()
main = do
  args <- getArgs
  listFileNames $
    case args of
      [pathArg] -> pathArg
      _ -> path

listFileNames :: FilePath -> IO ()
listFileNames fp =  getFiles path >>= putStrLn . intercalate ","


getFiles :: String -> IO [FilePath]
getFiles path =
  map (path </>) <$> listDirectory path
  >>=  foldr getFile (pure [])

data FPType = File | Directory

getFile :: FilePath -> IO [FilePath] -> IO [FilePath]
getFile f acc = liftM2 (++) acc $
    if takeExtension f == ".result"
    then return []
    else
      do
        mt <- getFPType f
        case mt of
            Nothing -> putStrLn (f ++ " is neither file nor directory" ) >> return []
            Just File -> return ["\"" ++ takeFileName f ++ "\""]
            Just Directory -> getFiles f

-- runMaybeT :: MaybeT m a -> m (Maybe a)
-- (<|>) :: f a -> f a -> f a
-- (<$) :: Functor f => a -> f b -> f a
-- liftIO :: MonadIO m => IO a -> m a
-- guard :: Alternative f => Bool -> f ()
getFPType :: FilePath -> IO (Maybe FPType)
getFPType fp = runMaybeT $ isFile <|> isDirectory
  where
    isFile :: MaybeT IO FPType
    isFile = File <$ (guard =<< liftIO (doesFileExist fp))
    isDirectory :: MaybeT IO FPType
    isDirectory = Directory <$ (guard =<< liftIO (doesDirectoryExist fp))
