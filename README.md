<a href="https://gamebanana.com/sprays/24171"><img alt="bowser" align="right" src="https://files.gamebanana.com/img/ico/sprays/bowserspray.png" width="100"></a>
<br>

# bowser
[![Build Status](https://travis-ci.org/mfaerevaag/bowser.svg?branch=tainted)](https://travis-ci.org/mfaerevaag/bowser)

Simple framework for testing security mechanisms in JavaScript interpreters.

## Running

The project uses stack, so to build, simply:

    # stack build

To run, try:

    # stack exec bowser-exe -- --file ./examples/fib_rec.js --threshold 1000000 --verbose

For more examples, see `examples/`.
