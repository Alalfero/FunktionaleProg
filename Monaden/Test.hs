module Main where
import HiddenTests
import PublicTests
import HaskellUnit

main = runTests  (tests++ts)
