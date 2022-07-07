#! /bin/tcsh -f
# foreach d (`ls -1d *`)
foreach d (`ls -1d *GeV*`)
  cd ${d}
  hadd.pl
  lsf hadd.xml
  cd -
end
