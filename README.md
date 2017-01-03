notate
======

An easy workflow for IHaskell notebooks using `hint`.

STATUS: Not functional yet. The kernel is the ihaskell demo "calc" kernel.

NOTE: This lifts whole the `ipython-haskell` subproject from https://github.com/gibiansky/IHaskell since there's some 8.0 issues in that project. For now, I've included the original license as `LICENSE.ihaskell` here, but I'm going to get rid of both ASAP.

Design
------

`jupyter` and `ihaskell` don't play nice with the `stack` model with their "user-global" registry of kernels and global ghc binaries. `notate` is a simple utility to isolate a registry for your stack project and provide a kernel via `hint`.

If not present, `notate install` creates a .notate folder in the current directory which should be put in your `.gitignore`. This will be used for jupyter configuration. The `notate` binary manages starting and stopping `hint` as well as `jupyter`.

These are set when running `jupyter`:

    HOME=$HOME
    JUPYTER_CONFIG_DIR=.notate/config
    JUPYTER_PATH=.notate/data
    JUPYTER_RUNTIME_DIR=.notate/runtime

Installation and Execution
--------------------------

First you must have `jupyter` installed and on your `PATH`.  You'll also need `libzmq` on your ld path. (ZMQ is used over the socket between `jupyter` and the kernel.) This can be done on Mac like

    pip install jupyter
    brew install zmq

In your project directory, where your cabal target is `myproject`, run

    stack exec notate -- install stack.yaml .notate
    stack exec notate -- notebook stack.yaml .notate

The arguments above are `STACK_YAML` and `CONFIG_DIR`.

You should then be able to evaluate notebooks as usual using the `notate` kernel.
