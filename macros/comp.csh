#! /bin/tcsh -f
set SourceFiles = $1;
set Opt = '-g -c';
set IncludePath = "-I. -I./.$STAR_HOST_SYS/include -I./StRoot -I$STAR/.$STAR_HOST_SYS/include -I$STAR/StRoot -I/usr/include/mysql  -I/afs/rhic.bnl.gov/star/ROOT/4.04.02/.sl305_gcc323/rootdeb/include";
echo $IncludePath
    g++ -c $Opt -pipe -Wall -W -Woverloaded-virtual -fPIC -Iinclude  -DR__QT -pthread $IncludePath $SourceFiles ;  
#g++ $ObjectFiles -shared -Wl,-soname,$LibName.so -g  $LinkedLibs -o $SharedLib"
