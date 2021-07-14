#!/bin/tcsh
starver SL21b

root4star -l -b -q $STAR/StRoot/macros/mudst/genDst.C\(-1,\"picoDst,PicoVtxMode:PicoVtxDefault,PicoCovMtxMode:PicoCovMtxWrite\",\"$1\"\)

