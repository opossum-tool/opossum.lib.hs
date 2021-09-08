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
import           Opossum.OpossumSPDXUtils
import           Opossum.OpossumScancodeUtils
import           Opossum.OpossumUtils

help :: IO ()
help = do
  putStrLn " ARG [ARG [ARG ...]]]"
  putStrLn "where ARG one of "
  putStrLn "            FILE           <-- parse opossum file"
  putStrLn "            DIR            <-- generate opossum from file tree"
  putStrLn " --spdx     SPDX_JSON      <-- parse .spdx.json"
  putStrLn " --spdx     SPDX_YAML      <-- parse .spdx.yaml"
  putStrLn " --scancode SCANCODE_JSON  <-- parse scancode json"

fun :: Opossum -> [String] -> IO Opossum
fun !pre []                      = return pre
fun !pre ("--spdx" : (f : args)) = do
  o <- parseSpdxToOpossum f
  fmap (pre <>) (fun o args)
fun !pre ("--scancode" : (f : args)) = do
  o <- parseScancodeToOpossum f
  fmap (pre <>) (fun o args)
fun !pre (f : args) = do
  fIsDirectory <- doesDirectoryExist f
  o            <- if fIsDirectory then opossumFromFileTree f else parseOpossum f
  fmap (pre <>) (fun o args)


main :: IO ()
main = getArgs >>= \case
  ["--help"] -> help
  args       -> do
    o <- fun mempty args

    hPutStrLn IO.stderr "normalize ..."
    let !normalizedO = normaliseOpossum o

    hPutStrLn IO.stderr "clusterify ..."
    let !clusterifiedO = clusterifyOpossum normalizedO

    writeOpossumStats clusterifiedO

    hPutStrLn IO.stderr "encode ..."
    C8.putStrLn (A.encodePretty clusterifiedO)
