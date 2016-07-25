#! /usr/local/bin/tcsh -f
foreach q (`ls -1d /star/data03/daq/2013/???/14* `)
  set r = `basename $q`
  set dd = `dirname $q`
  set day = `basename $dd`
  set d = $day/$r
  if (! -d $d) mkdir -p $d
  cd $d; echo $d; 
  daq_2013pp510W.pl 1
  if ($? && $? != 255) then
    /afs/rhic.bnl.gov/star/packages/.DEV2/scripts/star-submit ~/xml/daq_2013pp510W.xml; 
  endif
  cd -
end
