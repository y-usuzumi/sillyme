module SillyMe.Store.Engine.FileSystem where

import SillyMe.Store.Engine

data FileSystemEngine = FileSystemEngine { location :: FilePath
                                         }

instance Engine FileSystemEngine

