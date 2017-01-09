{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Options.Applicative

data Action = RecordSillyMistake { lang          :: Text
                                 , codeSnippet   :: Text
                                 , sillyCategory :: Text
                                 } deriving Show

recordSillyMistake :: Parser Action
recordSillyMistake = do
  lang <- T.pack <$> argument str (metavar "LANG")
  codeSnippet <- T.pack <$> argument str (metavar "CODE")
  sillyCategory <- T.pack <$> argument str (metavar "CATEGORY")
  return RecordSillyMistake { lang
                            , codeSnippet
                            , sillyCategory
                            }

main :: IO ()
main = do
  action <- execParser opts
  print action
  where
    opts = info (helper <*> recordSillyMistake)
      ( fullDesc
     <> header "Record your silly mistakes when coding" )
