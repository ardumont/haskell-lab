module HuffmanTests where

import Huffman

import Test.Framework
import Test.HUnit

testWeight1 = TestCase (assertEqual "Leaf" 2 $ weight (Leaf 'c' 2))
testWeight2 = TestCase (assertEqual "Fork" 5 $ weight (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5))

testWeights = TestList [TestLabel "testWeight1" testWeight1,
                        TestLabel "testWeight2" testWeight2]

-- *HuffmanTests> runTestTT testWeights
-- Loading package array-0.4.0.1 ... linking ... done.
-- Loading package deepseq-1.3.0.1 ... linking ... done.
-- Loading package HUnit-1.2.5.2 ... linking ... done.
-- Loading package old-locale-1.0.0.5 ... linking ... done.
-- Loading package time-1.4.0.1 ... linking ... done.
-- Loading package containers-0.5.0.0 ... linking ... done.
-- Loading package bytestring-0.10.0.2 ... linking ... done.
-- Loading package transformers-0.3.0.0 ... linking ... done.
-- Loading package mtl-2.1.2 ... linking ... done.
-- Loading package regex-base-0.93.2 ... linking ... done.
-- Loading package regex-posix-0.95.2 ... linking ... done.
-- Loading package unix-2.6.0.1 ... linking ... done.
-- Loading package ansi-terminal-0.6 ... linking ... done.
-- Loading package ansi-wl-pprint-0.6.6 ... linking ... done.
-- Loading package hostname-1.0 ... linking ... done.
-- Loading package random-1.0.1.1 ... linking ... done.
-- Loading package text-0.11.3.1 ... linking ... done.
-- Loading package xml-1.3.13 ... linking ... done.
-- Loading package test-framework-0.8.0.3 ... linking ... done.
-- Cases: 2  Tried: 2  Errors: 0  Failures: 0
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
