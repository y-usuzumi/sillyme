module SillyMe.Store.Engine where

class Engine e where


data FileSystemEngine = FileSystemEngine { location :: FilePath
                                         }

instance Engine FileSystemEngine
