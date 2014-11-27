echo This script is to build HelloWord against of Qt 3.x 
echo To build the example against of Qt 4.x use HelloWord.qt4.csh instead
g++ `root-config  --cflags --glibs` -I$QTDIR/include  -L$QTDIR/lib  -lqt-mt HelloWord.cxx -o HelloWord
