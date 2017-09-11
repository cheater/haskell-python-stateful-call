{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import System.IO (stdout)
import qualified Control.Exception as E

-- All the following modules would normally be imported as Py, but I imported
-- them under unique names for added clarity.
import qualified CPython as Py
import qualified CPython.Types.Module as PyM
import qualified CPython.Types.Exception as PyE
import qualified CPython.Protocols.Object as PyO
import qualified CPython.Types as PyT

main :: IO ()
main = do
  initCPython
  statefulDemo
  putStrLn "Hello, World!"

statefulDemo :: IO ()
statefulDemo = E.handle onException $ do
  pymain <- PyM.importModule "pymain"
  stateful <- pymain --> "stateful"
  let
    callStateful num = PyT.toInteger num >>=
      \n -> PyO.callArgs stateful [PyO.toObject n]
  callStateful 0
  callStateful 0
  callStateful 0
  callStateful 1
  callStateful 1
  callStateful 1
  return ()

initCPython :: IO ()
initCPython = do
  Py.initialize
  sys <- PyM.importModule "sys"
  path <- sys --> "path"
  PyO.print path stdout
  pathInsert <- path --> "insert"
  newPath <- PyT.toUnicode ""
  idx <- PyT.toInteger 0
  _ <- PyO.callArgs pathInsert [PyO.toObject idx, PyO.toObject newPath]
  PyO.print path stdout


getAttr obj attr = PyO.getAttribute obj =<< PyT.toUnicode attr
(-->) = getAttr -- requires NoMonomorphismRestriction

onException :: PyE.Exception -> IO ()
onException exc = PyO.print (PyE.exceptionValue exc) stdout
