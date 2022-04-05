
rm -f .runit.sh
cjobs
cjobs -format "%s\n" Cmd > .runit.sh
condor_rm zhux
cat .runit.sh
source .runit.sh

