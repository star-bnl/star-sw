#! /usr/local/bin/tcsh -f
set list  = `find . -type f`;
set files = `grep -l '\/usr\/local\/bin\/perl'  $list`
foreach file ($files)
  echo $file;
  if (-f tmp) rm tmp;
  sed -e 's/\/usr\/local\/bin\/perl/\/opt\/star\/bin\/perl/g'  $file > tmp 
  diff $file tmp
  mv tmp $file
end
# end of script
