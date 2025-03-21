# Testing Strategies

A summary of testing strategies for Ligo and SmartPy as Lltz changes over time.

## Ligo 

TODO 

## SmartPy 

SmartPy testing consists of unit tests and regression tests (the regressions are referred to as baselines).  Whilst the unit test coverage is pretty sparse the baselines' regression coverage is very wide and thorough.

Baseline testing covers all aspects of the compiler pipeline (parsing, typechecking, compilation and simulation) for local SmartPy installations of native code execution and Javascript code execution via node.  The native code regressions also test both the SmartPy simulator and the Octez mockup simulator.  These tests are run automatically as part of the SmartPy CI process on every push to the SmartPy code repository.  Any changes to the baseline output files are considered an error. 

### SmartPy - output based regression testing

The intention for SmartPy testing with Lltz would be to add a new set of baselines that run the compiler pipeline but using Lltz to generate the final Michelson code.  This can then be run through the SmartPy mockup simulator, which uses Octez to run Michelson code directly.  The output of these tests can be compared to the current mockup results by comparing e.g. the serialised contract storage data, simulated gas usage and serialised input parameter values.  Intermediate scenario values could also be serialised and compared.

### SmartPy - can we compare compilation of small expressions directly?

It may also be possible to compare some round trip small-expression compilations too.  We currently have tooling for doing 

SmartPy syntax -> Michelson -> SmartPy Michel (an intermediate language) -> LLTZ

which could be compared to the direct compilation:

SmartPy syntax -> LLTZ

### SmartPy - add to CI

These approaches would be added to the SmartPy CI and run on every push, then when Lltz gets updated we can 

* update the `opam pin` 
* rerun the baselines
* investigate any changes.
