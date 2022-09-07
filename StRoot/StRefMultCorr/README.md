# StRefMultCorr
A set of classes and functions to perform centrality selection in STAR.

## Usage example
Will be added later.


## Work in a standalone mode
StRefMultCorr could be compiled and run in a standalone mode with ROOT6.
To do that one can use Makefile calling:
```
make -jN
```
where *-jN* option forces to compile using N cores. This option may be ommited.

**IMPORTANT NOTE**
To compile StRefMultCorr in a standalone mode one **MUST** make sure that C++ standard
used in the makefile is exactly the same as the standard used to build ROOT.