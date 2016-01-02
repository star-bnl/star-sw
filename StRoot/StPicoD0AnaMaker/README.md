##Template analysis code for reading pico D<sup>0</sup> production
LBNL - STAR Experiment, Relativistic Heavy Ion Collider (RHIC), BNL  
RHIC year 2014 Run, with Heavy Flavor Tracker
  
###Code Authors:  
	Mustafa Mustafa (mmustafa@lbl.gov)  

- - -
### Presentations:  
#### STAR Protected:  
- [Introduction to data production and pipeline](http://www.star.bnl.gov/protected/heavy/mstftsm/run14/talks/2015-04-02.pdf), Mustafa, HF PWG, 2014-04-02  

- - -
###How to build this code:  
```bash
mkdir myAnalysis
cd myAnalysis

# Clone LBNL PicoHFLib
# Replace address below with your own fork if you have one
git clone https://github.com/rnc-lbl/auau200GeVRun14.git

# Now you need to get StPicoDstMaker
# If compiling at PDSF you need to get a klog token as below.
# - You don't need this step at RCF - 
# You will need to enter your RCF password.
klog -principal YOURRCFUSERNAME

# For pico production II
cvs co -r Run14_AuAu200_physics2 offline/users/dongx/pico/source/StPicoDstMaker

# Clone StRefMultCorr
git clone git@github.com:GuannanXie/Run14AuAu200GeV_StRefMultCorr.git

# Link all needed code under one StRoot directory:
mkdir StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoD0AnaMaker StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoD0EventMaker StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoKFVertexFitter StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoPrescales StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoHFMaker StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoCutsBase StRoot
ln -s `pwd`/offline/users/dongx/pico/source/StPicoDstMaker StRoot
ln -s `pwd`/Run14AuAu200GeV_StRefMultCorr/VPDMB5/StRefMultCorr StRoot
cp -r -p auau200GeVRun14/run14AuAu200GeVPrescales/ .

# Compile
starver SL15c
cons
```

###How to get a list of files:  
```bash
# Clone the file list repo:
git clone https://github.com/rnc-lbl/fileLists.git

# The list of daily D0 production will be under:
ls fileLists/Run14/AuAu/200GeV/physics2/picoD0Lists/daily

# To update your local copy of the list of files (recommended to do daily):
git pull origin master

# You need to link to the bad runs list in myAnalysis
ln -s fileLists/Run14/AuAu/200GeV/physics2/picoLists/picoList_bad_MB.list
```

###How to run this code:  
```bash
# For testing we can run the code on one file:
tail -n1 fileLists/Run14/AuAu/200GeV/physics2/picoD0Lists/daily/picoD0List_2015-05-21.list > test.list
ln -s `pwd`/auau200GeVRun14/StRoot/macros/runPicoD0AnaMaker.C
root4star -l -b -q -x runPicoD0AnaMaker.C\(\"test.list\",\"test_out\"\)
```

###How to submit jobs:
```bash
# You cah find STAR Scheduler XML file under:
cp -p auau200GeVRun14/starSubmit/submitPicoD0AnaMaker.xml .
# auau200GeVRun14/starSubmit/uREADME contains a how to use.
```
