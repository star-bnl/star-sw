#!/bin/sh
echo $QTROOTSYSDIR
if test "x$QTROOTSYSDIR" = "x";  then
  g++ `root-config  --cflags --glibs`   -I$QTDIR/include  -L$QTDIR/lib  -lqt-mt HelloPixmap.cxx -o HelloPixmap
else
  g++ -I$QTROOTSYSDIR/include  -L$QTROOTSYSDIR/lib `root-config  --cflags --glibs`  -I$QTDIR/include  -L$QTDIR/lib  -lQtRootGui -lqt-mt HelloPixmap.cxx -o HelloPixmap
fi
./HelloPixmap
