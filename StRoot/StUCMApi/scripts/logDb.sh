!#/bin/bash
defaultmsg="\"2009-05-11 20-09-15\" event=\"com.txcorp.ucm.log.event\" broker.job.id=\"0\" broker.task.id=\"orphan\" requester.name=\"fine\" context=\"UCM test Context\" level=\"3\" stage=\"1\" key=\"com.txcorp.ucm.app.start\" value=\"application started\""
netutil="wget -b -O test.html"
if [ "x$*"="x" ]; then 
   message=${defaultmsg}
else
   message=$*
fi
server=http://connery.star.bnl.gov/ucm/?m=${message}
echo Try: ${netutil} \'${server}\'
${netutil} \'${server}\'
echo Done!
