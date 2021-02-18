#! /bin/csh -f 
foreach f (`PicoL.pl`)
  set d = `echo ${f} | awk -F: '{print $2}'`
  set log = `echo ${f} | awk -F: '{print "Chain_"$3}' |sed -e 's/root/log/g' `
  if (-r ${log}) continue;
  root.exe -q -b 'Chain.C+("'${d}'/*picoDst.root","PicoDst")' | tee ${log}
end
