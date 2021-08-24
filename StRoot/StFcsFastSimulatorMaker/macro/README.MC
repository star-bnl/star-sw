This is obsolete. See https://www.star.bnl.gov/protected/spin/akio/fcs/howto_MC_github.html

*** How to get codes, compile and run for MC

stardev
tokens
(make sure you have token for afs, if not klog)
<cd to your dir>
cvs co StRoot/RTS
cvs co StRoot/StSpinPool/StFcsEventDisplay
cvs co StRoot/StSpinPool/StFcsTrgQaMaker
cvs co StRoot/StSpinPool/StFcsTriggerSimMaker
cvs co StRoot/StFcsFastSimulatorMaker/macro
cp StRoot/StFcsFastSimulatorMaker/macro/???* .
ln -s StRoot/StSpinPool/StFcsTriggerSimMaker/mask
cons

starsim -w 1 -b fcs.kumac
exit

root4star -b -q runSimFlat.C'(1,1,"mu+",30,0,0,100)' > log
root4star -b -q runSimBfc.C'(1,1,"mu+",30)' > log2
display mu+.e30.vz0.run1.0.eventDisplay.png 
(ctrl-c)

** Running single particle simmulation 

root4star -b -q runSimFlat.C'(10,1,"mu-",30,0,0,100)'
root4star -b -q runSimBfc.C'(10,1,"mu-",30)'

root4star -b -q runSimFlat.C'(10,1,"pi-",30)'
root4star -b -q runSimBfc.C'(10,1,"pi-",30)'

root4star -b -q runSimFlat.C'(10,1,"gamma",30)'
root4star -b -q runSimBfc.C'(10,1,"gamma",30)'

root4star -b -q runSimFlat.C'(10,1,"pi0",30)'
root4star -b -q runSimBfc.C'(10,1,"pi0",30)'

runsim 1000 1000 gamma 30 0 0 . 

(submit many jobs)
submitsim.pl 
submitsim.pl submit
mergesim.pl

*** Run starsim alone via kumac file
starsim
exec fcs.kumac pid=8
exec fcs.kumac#topview2
exec fcs.kumac#ecal
exec fcs.kumac#etowy
exec fcs.kumac#hcalc
exec fcs.kumac#htowz
exec fcs.kumac#htowy

*** See 3d drawing in ROOT
root StarVMC/Geometry/macros/loadAgML.C
draw("CAVE")

*** check overlap
root.exe
.L $STAR/StarVMC/Geometry/macros/loadStarGeometry.C
loadStarGeometry("test")
gGeoManager->CheckOverlaps()
new TBrowser()

*** View geometry
root.exe
.L StarVMC/Geometry/macros/viewStarGeometry.C
nocache=true
viewall=true
viewStarGeometry("test")

*** Runnning PYTHIA6 with filter and then run BFC
root4star runPythia.C()
