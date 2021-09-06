-- SPDX-FileCopyrightText: Maximilian Huber
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as C8
import Opossum.Opossum
import Opossum.OpossumUtils
import Opossum.OpossumSPDXUtils
import Opossum.OpossumScancodeUtils

help :: IO ()
help = do
    putStrLn " --merge-opossums FILE [FILE [...]]"
    putStrLn " --spdx-to-opossum SPDX_JSON"
    putStrLn " --spdx-to-opossum SPDX_YAML"
    putStrLn " --scancode-to-opossum SCANCODE_JSON"
    putStrLn " --opossum-from-filetree DIR"

main :: IO ()
main = getArgs >>= \case
    "--merge-opossums":args -> computeMergedOpossum args >>= C8.putStrLn
    "--spdx-to-opossum":[spdx] -> parseSpdxToOpossum spdx >>= C8.putStrLn
    "--scancode-to-opossum":[spdx] -> parseScancodeToOpossum spdx >>= C8.putStrLn
    "--opossum-from-filetree":[dir] -> opossumFromFileTree dir >>= C8.putStrLn
    _ -> help
