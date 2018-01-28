module Grap where

import AST
import Parser
import Matcher
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), stdin, stderr, hPutStr, withFile,
                  hGetContents)

process :: RE -> String -> Handle -> IO ()
process r fn h =
  do fc <- hGetContents h
     mapM_ (\s -> if null (matchTop r s) then return ()
                  else putStr $ fn ++ s ++ "\n")
           (lines fc)

main =
  do as <- getArgs
     case as of
       [] -> hPutStr stderr "Usage: Grap REGEXP [FILE]...\n"
       (pat:files) ->
         case parseString pat of
           Left e -> hPutStr stderr $ "Grap: " ++ e ++ "\n"
           Right r ->
             if null files then process r "" stdin
             else mapM_ (\fp -> let pref = if length files > 1 then fp ++ ":"
                                           else ""
                                in withFile fp ReadMode (process r pref)) files
