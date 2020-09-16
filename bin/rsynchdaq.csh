#! /usr/bin/tcsh -f
#foreach l (`ls -1d ./RunXXd.adc`)
foreach l (`ls -1d ./RunXXe.adc`)
#  set d = `basename ${l} .list.selected`:
#  if (! -d ${d}) mkdir ${d}
#  cd ${d}
  foreach f (`awk '{print $2}' ${l}`)
    set b = `echo ${f} | sed -e 's/\/gpfs01\/star\//\/hlt\/cephfs\//'`
    echo "$f => ${b}"
    rsync -avrz -h     rftpexp01.rhic.bnl.gov:${f} ${b}
  end
  cd -
end
