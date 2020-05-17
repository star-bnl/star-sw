#! /usr/bin/tcsh -f
foreach l (`ls -1d ../*2020.list.selected`)
  set d = `basename ${l} .list.selected`
  if (! -d ${d}) mkdir ${d}
  cd ${d}
  foreach f (`awk '{print $2}' ../${l}`)
  rsync -avrz -h     rftpexp01.rhic.bnl.gov:${f} .
  end
  cd -
end
