#!/bin/csh -f
if ($#argv == 0) then
  echo "Remove Windows CR"
  echo Usage: `basename $0` list_of_files
  exit
endif
  echo $*
foreach f ( $* )
  set name=`basename $f`
  echo "Convert $f"
  tr '\r' ' ' <$f >t$$; mv $f $f.BAK; mv t$$ $f
end
