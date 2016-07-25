#! /usr/local/bin/tcsh -f
set dir = $1
#set dest = $2
echo "copy from HPSS dir = $dir"# file = $dest"
cd /star/data05/reco
if (! -d $dir ) mkdir -p $dir
cd $dir
pftp -iv hpss 2121 << _EOD_
cd /home/starreco/reco/$dir
bin
pwd
dir *.dst.root
mpget *.dst.root
bye
_EOD_
