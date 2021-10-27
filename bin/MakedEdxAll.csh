 foreach d (`ls -1d *`)
  cd ${d}
  hadd.pl
  lsf hadd.xml
  cd -
end
