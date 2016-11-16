notate
======

An easy workflow for IHaskell notebooks using intero.

STATUS: Not functional yet. The kernel is the ihaskell demo "calc" kernel, not intero.

NOTE: This lifts whole the `ipython-haskell` subproject from https://github.com/gibiansky/IHaskell since there's some 8.1 issues in that project. For now, I've included the original license as `LICENSE.ihaskell` here, but I'm going to get rid of both ASAP.

Design
------

`jupyter` and `ihaskell` don't play nice with the `stack` model with their "user-global" registry of kernels and global ghc binaries. `notate` is a simple utility to isolate a registry for your stack project and provide a kernel via `intero`.

If not present, `notate install` creates a .notate folder in the current directory which should be put in your `.gitignore`. This will be used for jupyter configuration. The `notate` binary manages starting and stopping `intero` as well as `jupyter`.

These are set when running `jupyter`:

    HOME=$HOME
    JUPYTER_CONFIG_DIR=.notate/myproject/config
    JUPYTER_PATH=.notate/myproject/data
    JUPYTER_RUNTIME_DIR=.notate/myproject/runtime

It should be simple enough to read/write to intero stdin/stdout for cell evaluation, but how will client code emit structured data like images? The `ihaskell` approach is to bake all "display data" formats into the kernel and do special processing of `ghci` output.  This doesn't scale well, for obvious reasons. As horrible as it sounds, we can do IO tricks to make this problem less bad. Client code can write a JSONL-formatted Array of DisplayData structs (MIME type and encoded content pairs) to a file path specified in a particular environment variable, `NOTATE_DISPLAY_PATH`. (NOTE: JSONL is simply a sequence of JSON documents separated by newlines.) It's a hack but it should work.  Obviously it's probably best to provide a lightweight `notate-client` library to handle the details.  Client code can manage that and other specific format adaptors (images, plots, tables, HTML) in their `cabal` dependencies.

Installation and Execution
--------------------------

First you must have `jupyter` installed and on your `PATH`.  You'll also need `libzmq` on your ld path. (ZMQ is used over the socket between `jupyter` and the kernel.) This can be done on Mac like

    pip install jupyter
    brew install zmq

In your project directory, where your cabal target is `myproject`, run

    stack install intero
    stack install notate
    notate install . .notate myproject
    notate notebook . .notate myproject

The arguments above are `PROJECT_DIR`, `CONFIG_DIR`, and `TARGET`.

You should then be able to evaluate notebooks as usual using the `notate` kernel.
