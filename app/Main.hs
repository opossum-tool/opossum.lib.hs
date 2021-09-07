-- SPDX-FileCopyrightText: Maximilian Huber
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)
import qualified System.FilePath as FP
import qualified Data.ByteString.Lazy.Char8 as C8
import System.Directory (doesDirectoryExist)
import qualified Data.Aeson.Encode.Pretty as A

import Opossum.Opossum
import Opossum.OpossumUtils
import Opossum.OpossumSPDXUtils
import Opossum.OpossumScancodeUtils

help :: IO ()
help = do
    putStrLn " ARG [ARG [ARG ...]]]"
    putStrLn "where ARG one of "
    putStrLn "FILE                       <-- parse opossum file"
    putStrLn "DIR                        <-- generate opossum from file tree"
    putStrLn " --spdx SPDX_JSON          <-- parse .spdx.json"
    putStrLn " --spdx SPDX_YAML          <-- parse .spdx.yaml"
    putStrLn " --scancode SCANCODE_JSON  <-- parse scancode json"

fun :: Opossum -> [String] -> IO Opossum
fun pre []                         = return pre
fun pre ("--spdx": (f : args))     = do
    o <- parseSpdxToOpossum f
    fmap (pre <>) (fun o args)
fun pre ("--scancode": (f : args)) = do
    o <- parseScancodeToOpossum f
    fmap (pre <>) (fun o args)
fun pre (f :args)                  = do
    fIsDirectory <- doesDirectoryExist f
    o <- if fIsDirectory
         then opossumFromFileTree f
         else parseOpossum f 
    fmap (pre <>) (fun o args)
    

main :: IO ()
main = getArgs >>= \case
    ["--help"] -> help
    args       -> do 
      o <- fun mempty args
      C8.putStrLn (A.encodePretty (clusterifyOpossum o))
