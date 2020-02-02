#!/bin/tcshrc -f 
  setenv noLD ""
  if ("$STARCMPL" == "icc") then
    setenv CC icc
    setenv CXX icc
    setenv F77 ifort
    setenv noLD "--without-gnu-ld"
  else 
    unsetenv CC
    unsetenv CXX
    unsetenv F77
  endif
./configure --prefix=/afs/rhic/star/users/fisyak/public/sources/mysql  --exec-prefix=/afs/rhic/star/users/fisyak/public/sources/mysql/.${STAR_HOST_SYS} ${noLD}
