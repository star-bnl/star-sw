##Event mixer template for reconstructing D<sup>0</sup> foreground and background from 
LBNL - STAR Experiment, Relativistic Heavy Ion Collider (RHIC), BNL  
RHIC year 2014 Run, with Heavy Flavor Tracker
  
###Code Authors:  
	**Michael Lomnitz (mrlomnitz@lbl.gov)
	Mustafa Mustafa (mmustafa@lbl.gov)  
	** Code Maintainer
- - -
### Presentations:  
#### STAR Protected:  
- [Introduction to event mixer](http://www.star.bnl.gov/protected/heavy/mlomnitz/PicoMixedEvent/PicoMixedEvent.pdf), M. Lomnitz, HF PWG, 2014-07-16  

- - -
###How to build this code:  
```bash
mkdir myMixedEvent
cd myMixedEvent

# Clone LBNL PicoHFLib
# Replace address below with your own fork if you have one
git clone https://github.com/rnc-lbl/auau200GeVRun14.git

# For pico production II
# Clone picoDst maker from git repository
# Replace address below with your own fork if you have one
git clone git@github.com:rnc-lbl/star-picoDst.git

# Clone StRefMultCorr
git clone git@github.com:GuannanXie/Run14AuAu200GeV_StRefMultCorr.git

# Link all needed code under one StRoot directory:
mkdir StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoMixedEventMaker StRoot
ln -s `pwd`/star-picoDst/StPicoDstMaker StRoot/StPicoDstMaker
ln -s `pwd`/Run14AuAu200GeV_StRefMultCorr/VPDMB5/StRefMultCorr StRoot

# Compile
starver SL15c
cons
```

###How to get a list of files:  
```bash
# Clone the file list repo:
git clone https://github.com/rnc-lbl/fileLists.git

# To update your local copy of the list of files (recommended to do daily):
git pull origin master

# You need to link to the bad runs list in myAnalysis
ln -s fileLists/Run14/AuAu/200GeV/physics2/picoLists/picoList_bad_MB.list
```

###How to run this code:  
```bash
# For testing we can run the code on one file:
ln -s `pwd`/auau200GeVRun14/StRoot/macros/runPicoMixedEvent.C
root4star -l -b -q -x runPicoMixedEvent.C\(\"test.list\",\"test_out\"\)
```

###How to submit jobs:
```bash
# You cah find STAR Scheduler XML file under:
cp -p auau200GeVRun14/starSubmit/submitPicoMEMaker.xml .
cp -p auau200GeVRun14/starSubmit/submitPicoMEMaker.csh .
# Change the basefolder path in submitPicoMEMaker.csh to your local directory and the appropriate file list.
# Submit the jobs with the following
csh submitPicoMEMaker.csh
```
