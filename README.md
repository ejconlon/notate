notate
======

An easy workflow for IHaskell notebooks using intero.

NOTE: This lifts whole the `ipython-haskell` subproject from https://github.com/gibiansky/IHaskell (since there's some 8.1 issues in that project).

First you must have jupyter installed and on your PATH.  You'll also need libzmq on your ld path.

In your project directory, run

    stack install intero
    stack install notate
    stack exec notate

If not present, notate creates a .notate folder in the current directory which should be gitignored. This will be used for jupyter configuration. The notate binary manages starting and stopping intero as well as jupyter.

These are set when running jupyter:
JUPYTER_CONFIG_DIR=.notate/config
JUPYTER_PATH=.notate/data
JUPYTER_RUNTIME_DIR=.notate/runtime
