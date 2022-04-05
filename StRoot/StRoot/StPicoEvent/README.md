# StPicoEvent

The StPicoEvent set of classes (library) allows to store and work with *filename.picoDst.root* files or with list of such files.

************************************************************

Maintainer:   Grigory Nigmatkulov

Institution:  National Research Nuclear University MEPhI

Date:         June 8, 2018

E-mail:       nigmatkulov@gmail.com ; ganigmatkulov@mephi.ru

Contributors:
- Xin Dong
- Mustafa Mustafa
- Dmitri Smirnov
- Jerome Lauret
- Yuri Fisyak
- Gene Van Buren
- Mike Lisa
- Rongrong Ma
- Peng Liu
- Florian Seck
- Justin Ewigleben
- Grigory Nigmatkulov
- and others

************************************************************

Brief description of how to compile and run the analysis
over picoDst on your laptop and/or RACF.

## Installation

a) System has to have ROOT preinstalled (should work with versions 5 and 6).

b) There is a Makefile stored in the **_StPicoEvent_** directory. In order to compile the codes one needs to run:

```
make
```

c) Since it is the most commonly used, the **g++** compiler is used for the
compilation. However, one can also switch it to **clang++** (has been successfully tested)

d) After the compilation is finished the shared library *libStPicoDst.so* will be
created.

e) If you have some errors during the compilation please contact the picoDst
maintainer (and/or picoDst mailing list).

## Processing picoDst

There are three ROOT macroses with example of how to perform a simple analysis using picoDst. They are stored in the **StPicoEvent/macros** directory and called: *PicoDstAnalyzer.C*, *RunAnalyzer.C*, and *SimplePicoDstAnalyzer.C*.

Lets assume that one has and input file(s) *InputFile* with a *name.picoDst.root* or a list of picoDst files, called *name.lis(t)*, and StPicoEvent compiled (i.e. the *libStPicoDst.so* library exists).

There are 2 possible processing scenarios of using StPicoEvent library and classes depending on the ROOT version:

### ROOT 5:

One should run ROOT from the terminal:

```
[myterm]> root RunAnalyzer.C\(\"InputFile\"\)
```

Or run this macros from the interactive session:
```
[myterm]> root
root [0].x RunAnalyzer.C("InputFile")
```

Any of aforementioned ways load *libStPicoDst.so*, compile and run *RunAnalyzer.C*. After the processing the remove dictionary and library created by ACLiC.

### ROOT 6:

Since ROOT 6 does not have CINT there is some extra flexibility on how to analyze the data. The first one is listed above. The second option is to run the *PicoDstAnalyzer.C* macro directly.

Either from the terminal:

```
root PicoDstAnalyzer.C\(\"InputFile"\)
```

Or from the root session:

```
[myterm]> root
root [0].x PicoDstAnalyzer.C("InputFile")
```

### Simple Processing

The other possibility is not to use **StPicoEvent** classes, but read *filename.picoDst.root* files as regular ROOT TTree. The macros *SimplePicoDstAnalyzer.C* shows an example of doing it.

## Troubleshooting

For any questions or with any suggestions please contact the package maintainer and also discuss it in the STAR picoDst mailing list: **picodst-hn@sun.star.bnl.gov**
