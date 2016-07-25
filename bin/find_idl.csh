#! /usr/local/bin/tcsh -f
set list = `ls *.inc`;
foreach idl ($list) 
  echo $idl
  find  /afs/rhic/star/users/fisyak/e896/all -name $idl -exec ls -l {} \;
#  find  /afs/rhic/star/users/fisyak/e896/all -name $idl:r.h -exec ls -l {}  \;
#  find  /afs/rhic/star/users/fisyak/e896/all -name $idl:r.inc -exec ls -l {}  \;
    echo "--------------------"
end
#end_of script 
