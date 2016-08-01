#!/bin/tcsh -f
if (-r hold) rm hold
touch hold
foreach f ( `condor_q -wide -submitter fisyak | grep -w H | awk '{print $9}'` )
ls -1d ${PWD}/${f} >> hold
end
set l = `awk -F_ '{print $2}' hold | awk -F\. '{print $1}' | xargs | sed -e 's/ /,/g'`
echo $l
rm hold
