#!/usr/bin/env runhaskell
import System.Process
import System.Exit
import Control.Monad

succeed a = do
    ec <- a
    unless (ec==ExitSuccess) (exitWith ec)

main = do
    succeed $ rawSystem "ghc" [
        "--make","Main.hs",
        "-outputdir","outputdir",
        "-i..",
        "-fforce-recomp",
        "-hide-all-packages",
        "-package","base",
        "-package","template-haskell",
        "-package","syb",
        "-package","containers"]
    succeed $ system "./Main"

