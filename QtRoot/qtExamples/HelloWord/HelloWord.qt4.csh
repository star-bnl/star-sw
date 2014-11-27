g++ `root-config  --cflags --glibs` -I$QTDIR/include  -DQT_GUI_LIB -DQT_CORE_LIB  -L$QTDIR/lib -lQtCore -lQtGui HelloWord.cxx -o HelloWord
ls -l HelloWord
echo 'Type "HelloWord" to launch the example'
