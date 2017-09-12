# Installation:

Before installing, please start a new `cabal sandbox` for this package and
install `c2hs` in the sandbox. You will also need the python 3.x headers for
the version of python you are using: e.g. `apt-get install python3.5-dev`.
After all prerequisites are installed, you can execute with `cabal run`. This
should be done from the top level of the git repository, since that's where the
python code (in the file `pymain.py`) is located, and the Haskell code will
look for it in the current working directory.


# About CPython:

I have decided to use the [CPython
package](https://hackage.haskell.org/package/cpython). While verbose, it is
uniquely suited to creating a clean, high level abstraction over Python
libraries, because it has a very good grip on the semantics of Python and
allows manipulating Python values as first class values in Haskell. This means
that it can convey the semantics of the Python library better than
quasiquoters, which end up treating all Python related code as text.

CPython documentation isn't verbose enough. However, once you get the hang of
its classes, you can make fast progress. One thing to bear in mind that in a
type declaration such as this:
    toInteger :: Integer -> IO Integer
the left `Integer` might be the usual Haskell `Integer` from the `Prelude`,
whereas the one on the right is a completely different `Integer` provided by
`CPython.Types`. This can be confusing and when viewing the haddocks you have
to hover the link to find out where the type comes from.

There is a fairly good tutorial, which can help understanding CPython when the
haddocks don't explain things too well: [Calling Python libraries from
Haskell](https://john-millikin.com/articles/ride-the-snake/). I suggest reading
that after this README and before reading the code. It's a quick read.


# Code description:

## Unusual coding conventions

In order to help understanding it, I've done a few things in this piece of code
I would have otherwise done differently:

- The way to use the CPython library is to import all modules to a single
  namespace called `Py`. Here, each module is imported under a different name,
so that it's easier to figure out where things came from.
- There are copious inline comments, which might be less verbose in day to day
  code.
- There is a lot of debugging output in order to illustrate what's happening on
  the interface of Haskell and Python.

## Code execution: environment setup

The code starts by setting up the Python environment and adding the current
working directory to Python import path. Next up, it imports the Python module
`pymain` which is in `pymain.py` and loads a function in it called `stateful`.
Subsequently, it calls that function multiple times with different arguments.

The function `stateful` contains what is essentially a static variable, i.e.
one that keeps its value across subsequent executions of the function. This
value, called `counter`, is initialized to `0` upon the first execution. The
function has an argument called `i`, which will be added to the counter. In the
end, the value of the counter is returned.

In addition to this, both the function `stateful` in Python, as well as the
Haskell code calling it, contain print statements that show what values the
function is returning.

## Calling the Python function from Haskell

The process of calling the function is a little involved. The function must be
selected as an attribute of the module that was imported. To make this less
verbose, I created an infix operator called `-->` that can select attributes.
The function is called with the arguments in a list, each of which must be
wrapped in the right Python type, and then in CPython's `Object` type which
essentially means the marshalling.

## Parsing and using the Python function's return value in Haskell

After the function returns, it gives us a value of type `SomeObject` which
illustrates the ambiguity of Python's typing: only once the value is used
somewhere, Python might cast it to a type that fits in the expression being
used. We have to do it "by hand" by calling `cast`, although in reality
Hindley-Milner type inference does all the work for us by, similarly to Python,
selecting the concrete type that will fit in the place where we are using the
value. Finally, once `SomeObject` has been cast to CPython's `Integer`, we
again convert it to a Haskell `Integer`. This value gets printed out.

## Utility functions

The code also contains a small exception handler which will print the message
of the exception. To try it out, you can change the string `"pymain"` in the
function `statefulDemo` to something else, so that Python tries to import a
module which does not exist.

I've introduced a function called `getAttr` with a related infix operator
called `-->` in order to make refering to Python attributes easier to
understand.
