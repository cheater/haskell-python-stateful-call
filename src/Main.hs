{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import System.IO (stdout)
import qualified Control.Exception as E
import Control.Monad (when)

-- All the following modules would normally be imported as Py, but I imported
-- them under unique names for added clarity.
import qualified CPython as Py
import qualified CPython.Types.Module as PyM
import qualified CPython.Types.Exception as PyE
import qualified CPython.Protocols.Object as PyO
import qualified CPython.Protocols.Number as PyN
import qualified CPython.Types as PyT

main :: IO ()
main = do
  initCPython True
  statefulDemo

-- | Call a stateful function from a Python module.
statefulDemo :: IO ()
statefulDemo = E.handle onException $ do
  pymain <- PyM.importModule "pymain"
  stateful <- pymain --> "stateful"
  let
    -- | Call the function "stateful" and evaluate to its return value.
    callStateful i = do
      Just pyInt <- PyT.toInteger i -- convert i to a Python int
        >>= \pyIntArg -> PyO.callArgs stateful [PyO.toObject pyIntArg] -- call
        -- callArgs returns a polymorphic type called SomeObject. It needs to
        -- be cast to a concrete type so that it can be used. This step is
        -- necessary to account for Python's dynamic typing, which would
        -- otherwise go unaccounted for in Haskell.
        >>= PyO.cast -- cast to type inferred elsewhere or fail with Nothing
      PyT.fromInteger pyInt -- this binds the cast above to a Python int and
                            -- converts that Python int to a Haskell Integer.

    printInt int = putStrLn $ "Haskell: " ++ show int

    callAndPrint int = callStateful int >>= printInt

  callAndPrint 0
  callAndPrint 0
  callAndPrint 0
  callAndPrint 1
  callAndPrint 1
  callAndPrint 1

-- | Initialize the CPython module and set up import path.
initCPython :: Bool -> IO ()
initCPython verbose = do
  -- initialize CPython bindings:
  Py.initialize

  -- add pwd to the Python path, so that it can find a module located in the
  -- current directory:
  sys <- PyM.importModule "sys"
  path <- sys --> "path"
  when verbose $ putStrLn "Current Python path:"
  when verbose $ PyO.print path stdout
  when verbose $ putStrLn "Adding pwd to Python path..."
  pathInsert <- path --> "insert"
  newPath <- PyT.toUnicode ""
  idx <- PyT.toInteger 0
  _ <- PyO.callArgs pathInsert [PyO.toObject idx, PyO.toObject newPath]
  when verbose $ PyO.print path stdout

-- Get attribute of object. Python's foo.bar is foo --> bar in our code.
getAttr obj attr = PyO.getAttribute obj =<< PyT.toUnicode attr
(-->) = getAttr -- requires NoMonomorphismRestriction

-- | Print an exception's message.
onException :: PyE.Exception -> IO ()
onException exc = PyO.print (PyE.exceptionValue exc) stdout
