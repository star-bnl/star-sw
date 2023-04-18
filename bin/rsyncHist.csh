#! /usr/bin/tcsh -f
if ($#argv != 1) then
    echo "Usage : $0 RunXIX" 
    exit
else 
    cd ~/work/Histograms
    echo "rsync -avrz -h  --include='*.root'  --exclude='*.*' $argv[1] /direct/gpfs01/star/subsys-tpc/fisyak/Histograms/"
    rsync -avrz -h  --include='*.root'  --exclude='*.*' $argv[1] /direct/gpfs01/star/subsys-tpc/fisyak/Histograms/ >& $argv[1].log &
endif

