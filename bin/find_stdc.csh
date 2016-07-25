#! /bin/csh -f 
set dirlist = "$OPTSTAR/qt/lib $ROOTSYS/lib"# $STAR_LIB"
foreach p ($dirlist)
set list = `echo ${p}/*.s*`
foreach lib ($list) 
   echo "lib = " $lib 
   ldd $lib | grep  libstdc 
end
end
#cd $cwd
