#!/bin/tcsh -f
if (-r hold) rm hold
touch hold
#foreach f ( `condor_q -wide -submitter fisyak | grep -w H | awk '{print $9}'` )
foreach f ( `condor_q -wide | grep -w H | awk -F\' '{print $3}'`)
#ls -1d ${PWD}/${f} >> hold
ls -1d ${f} >> hold.list
end
set l = `awk -F_ '{print $2}' hold.list | awk -F\. '{print $1}' | xargs | sed -e 's/ /,/g'`
echo $l
rm hold.list
#  lsf -r 129 4A46500935233CCB56450949D47CA985.session.xml
