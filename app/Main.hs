-- SPDX-FileCopyrightText: Maximilian Huber
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.ByteString.Lazy.Char8    as C8
import           System.Directory               ( doesDirectoryExist )
import           System.Environment             ( getArgs )
import qualified System.FilePath               as FP
import           System.IO                      ( Handle
                                                , hClose
                                                , hPutStrLn
                                                , stdout
                                                )
import qualified System.IO                     as IO

import           Opossum.Opossum
import           Opossum.OpossumDependencyCheckUtils
import           Opossum.OpossumExiftoolUtils
import           Opossum.OpossumSPDXUtils
import           Opossum.OpossumScancodeUtils
import           Opossum.OpossumScanossUtils
import           Opossum.OpossumUtils

help :: IO ()
help = do
  putStrLn " ARG [ARG [ARG ...]]]"
  putStrLn "    where ARG one of "
  putStrLn "     FILE                       <-- parse opossum file"
  putStrLn "     DIR                        <-- generate opossum from file tree"
  putStrLn "     --spdx SPDX_JSON           <-- parse .spdx.json"
  putStrLn "     --spdx SPDX_YAML           <-- parse .spdx.yaml"
  putStrLn "     --scancode SCANCODE_JSON   <-- parse scancode json"
  putStrLn
    "     --dependency-check DC_JSON <-- parse OWASP Dependency-Check JSON"
  putStrLn "     --scanoss SCANOSS_JSON     <-- parse scanoss json"
  putStrLn "     --exiftool EXIFTOOL_SJON   <-- parse exiftool json"
  putStrLn "or"
  putStrLn " --merge-relative OPOSSUM [OPOSSUM [OPOSSUM [...]]]"



main :: IO ()
main = getArgs >>= \case
  ["--help"] -> help
  args       -> do
    o <- case args of
      ("--merge-relative" : fs) ->
        let fun :: String -> IO Opossum
            fun f = do
              o <- parseOpossum f
              return (unshiftPathToOpossum (FP.takeDirectory f) o)
        in  fmap mconcat (mapM fun fs)
      _ ->
        let fun :: Opossum -> [String] -> IO Opossum
            fun !pre []                      = return pre
            fun !pre ("--spdx" : (f : args)) = do
              o <- parseSpdxToOpossum f
              fmap (pre <>) (fun o args)
            fun !pre ("--scancode" : (f : args)) = do
              o <- parseScancodeToOpossum f
              fmap (pre <>) (fun o args)
            fun !pre ("--scanpipe" : (f : args)) = do
              o <- parseScanpipeToOpossum f
              fmap (pre <>) (fun o args)
            fun !pre ("--dependency-check" : (f : args)) = do
              o <- parseDependencyCheckToOpossum f
              fmap (pre <>) (fun o args)
            fun !pre ("--scanoss" : (f : args)) = do
              o <- parseScanossToOpossum f
              fmap (pre <>) (fun o args)
            fun !pre ("--exiftool" : (f : args)) = do
              o <- parseExiftoolToOpossum f
              fmap (pre <>) (fun o args)
            fun !pre (f : args) = do
              fIsDirectory <- doesDirectoryExist f
              o            <- if fIsDirectory
                then opossumFromFileTree f
                else parseOpossum f
              fmap (pre <>) (fun o args)
        in  fun mempty args

    hPutStrLn IO.stderr "normalize ..."
    let !normalizedO = normaliseOpossum o

    hPutStrLn IO.stderr "clusterify ..."
    let !clusterifiedO = clusterifyOpossum normalizedO

    writeOpossumStats clusterifiedO

    hPutStrLn IO.stderr "encode ..."
    C8.putStrLn (A.encodePretty clusterifiedO)
