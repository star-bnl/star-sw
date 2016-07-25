#! /usr/local/bin/tcsh -f
set list = `ls *.log`
foreach f ($list)
    echo $f
    if (! -r ../$f) then
      echo "mv $f ../"
      mv $f ../
    else 
      ls -l $f ../$f
    endif
end
#end of script
