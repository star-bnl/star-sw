#!/bin/tcsh
echo "Compiling testUcm.cxx  . .  . "
  g++ -g -m32 -o testUcm -IStRoot  -I$OPTSTAR/include StRoot/logging/testUcm.C -LX86 -L.${STAR_HOST_SYS}/lib -L$OPTSTAR/lib -L/usr/lib/mysql -llogging -lmysqlclient -llog4cxx
  ls -l testUcm
echo "Compiling Main.cxx  . .  . "
  g++ -g -m32 -o Main -IStRoot  -I$OPTSTAR/include StRoot/logging/Main.cxx -LX86 -L.${STAR_HOST_SYS}/lib -L$OPTSTAR/lib -L/usr/lib/mysql -llogging -lmysqlclient -llog4cxx
  ls -l Main
echo Done . . . 
