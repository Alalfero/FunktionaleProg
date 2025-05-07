module HaskellUnit where

import           Control.Exception
import           Data.List
import           Data.Maybe
import           System.IO

esc '"'  = "&quot;"
esc '\'' = "&apos;"
esc '&'  = "&amp;"
esc '<'  = "&lt;"
esc '>'  = "&gt;"
esc c    = [c]

xmlEsc = concat . map esc

runTests tests = do
  putStrLn "<?xml version=\"1.0\" ?>"
  putStrLn "<testsuite>"
  xs <- sequence tests
  putStrLn "</testsuite>"

genericTestcase compareFunction name msg expected actual = do
  res <- compareFunction expected actual
  maybe
    (do
       putStrLn ("  <testcase name=\"" ++ name ++ "\">")
       hFlush stdout
       putStrLn "  </testcase>"
       hFlush stdout
       return True)
    (\failMessage -> do
       putStrLn ("  <testcase name=\"" ++ name ++ "\">")
       putStrLn ("    <failure message=\"" ++ failMessage ++ "\">")
       putStrLn (msg ++ ": " ++ failMessage)
       putStrLn "    </failure>"
       putStrLn "  </testcase>"
       return False)
    res

testcase :: (Eq a, Show a) => [Char] -> [Char] -> a -> a -> IO Bool
testcase = genericTestcase compareResult

valuetestcase :: (Eq a, Show a) => [Char] -> [Char] -> a -> a -> IO Bool
valuetestcase = genericTestcase compareValueResult

compareValueResult expected actual = do
  catch
    (if (expected == actual)
       then return Nothing
       else return
              (Just
                 (xmlEsc
                    ("<"
                       ++ show actual
                       ++ ">"
                       ++ " is not the expected solution."))))
    (\e ->
       return
         (Just
            (replace
               "\n"
               " "
               ("Exception in evaluation: " ++ (show (e :: SomeException))))))

compareResult expected actual = do
  catch
    (if (expected == actual)
       then return Nothing
       else return
              (Just
                 (xmlEsc
                    ("expected:<"
                       ++ show expected
                       ++ "> but was:<"
                       ++ show actual
                       ++ ">"))))
    (\e ->
       return
         (Just
            (replace
               "\n"
               " "
               ("Exception in evaluation: " ++ (show (e :: SomeException))))))

replace [] _ _ = error "Extra.replace, first argument cannot be empty"
replace from to xs
  | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []
