-- SPDX-FileCopyrightText: Maximilian Huber
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as C8
import Opossum.Opossum
import Opossum.OpossumUtils

help :: IO ()
help = do
    putStrLn " --merge-opossums FILE [FILE [...]]"

main :: IO ()
main = getArgs >>= \case
    "--merge-opossums":args -> computeMergedOpossum args >>= C8.putStrLn
    -- "--spdx-to-opossums":[spdx] -> parseSpdxToOpossum spdx >>= C8.putStrLn
    _ -> help
