{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest (defaultMainWithDoctests)
main :: IO ()
main = defaultMainWithDoctests "doctests"

#else

import Distribution.Simple
main :: IO ()
main = defaultMain

#endif
