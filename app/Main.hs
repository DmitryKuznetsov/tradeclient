module Main where

import Lib
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Lens
import Network.Wreq

{-data Options = Options  { optRunLoop    :: Int -> IO()}
startOptions :: Options
startOptions = Options  { optRunLoop    = return}
                        
options :: [ OptDescr (Options -> Options) ]
options =
    [ Option "r" ["run"]
        (ReqArg
            (\arg opt -> return opt { optRunLoop = someFunc arg}))
        "Run iteration"
    ]-}

main :: IO ()
main = do
  [arg1] <- getArgs
  --let max = read arg1 :: String
  x <- runInfoRequest "8F279AF215E31A092423FDB7F0CBAF8F" "4471dea081854a24a663fdc1c44bc608"
  print (x ^. responseBody)
  -- Parse options, getting a list of option actions
  {-let (actions, nonOptions, errors) = getOpt RequireOrder options args
 
  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return startOptions) actions
 
  let Options { {-optVerbose = verbose
              , optInput = input
              , optOutput = output-} optRunLoop = someFunc } = opts
 -}
  --when verbose (hPutStrLn stderr "Hello!")
  --when someFunc ()
  --input >>= output

  --when someFunc 10
  --someFunc
--  n <- someFunc
--  print n
