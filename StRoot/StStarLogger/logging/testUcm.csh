#!/bin/tcsh
echo "Compiling testUcm.cxx  . .  . "
  g++ -g -m32 -o testUcm -IStRoot  -I$OPTSTAR/include StRoot/logging/testUcm.C -L.${STAR_HOST_SYS}/lib -L$OPTSTAR/lib -L/usr/lib/mysql -llogging -lmysqlclient -llog4cxx
echo Done . . . 
ls -l testUcm
