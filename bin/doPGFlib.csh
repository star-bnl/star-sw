#!/bin/csh
# do PGF library. 

set pgf77A = (  allo.o fpcvt.o pgdummy.o cvt.o intrin.o pgifiodf.o fseek643f.o ftell643f.o)

if ( -e libpgf77A.a)  rm libpgf77A.a
if ( -e libpgf77S.so) rm libpgf77S.so


ar -xv ${PGILIB}/libpgftnrtl.a
ar -xv ${PGILIB}/libpgc.a
ar -xv ${PGILIB}/libpgnod_prof.a 


ar -cr libpgf77A.a $pgf77A
#rm $pgf77A

gcc -shared -o libpgf77S.so  *.o 
#${PGILIB}/libpgftnrtl.a ${PGILIB}/libpgc.a  ${PGILIB}/libpgnod_prof.a ${PGILIB}/libpgftnrtl.a


rm *.o
