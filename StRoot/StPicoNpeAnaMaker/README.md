##Template analysis code for reading pico NPE production
LBNL - STAR Experiment, Relativistic Heavy Ion Collider (RHIC), BNL  
RHIC year 2014 Run, with Heavy Flavor Tracker
  
###Code Authors:  
    **Kunsu OH (kunsuoh@gmail.com)  
    Mustafa Mustafa (mmustafa@lbl.gov)  

    ** Code maintainer

- - -
###How to build this code:  
```bash
mkdir myAnalysis
cd myAnalysis

# Clone LBNL PicoHFLib
# Replace address below with your own fork if you have one
git clone https://github.com/kunsuoh/auau200GeVRun14rnc.git auau200GeVRun14
git clone http://github.com/rnc-lbl/auau200GeVRun14.git

# Now you need to get StPicoDstMaker
# If compiling at PDSF you need to get a klog token as below.
# - You don't need this step at RCF - 
# You will need to enter your RCF password.
klog -principal YOURRCFUSERNAME
cvs co -r Run14_AuAu200_physics2 offline/users/dongx/pico/source/StPicoDstMaker

# Link all needed code under one StRoot directory:
mkdir StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoNpeAnaMaker StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoNpeEventMaker StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoHFMaker StRoot
ln -s `pwd`/auau200GeVRun14/StRoot/StPicoPrescales StRoot
ln -s `pwd`/offline/users/dongx/pico/source/StPicoDstMaker StRoot
cp -r -p auau200GeVRun14/run14AuAu200GeVPrescales/ .

# Compile
starver SL15c
cons
```

###How to get a list of files:  
```bash
# Clone the file list repo:
git clone https://github.com/rnc-lbl/fileLists.git

# The list of daily NPE production will be under:
ls fileLists/Run14/AuAu/200GeV/physics2/picoNpeLists/daily

# To update your local copy of the list of files (recommended to do daily):
git pull origin master

# You need to link to the bad runs list in myAnalysis
ln -s fileLists/Run14/AuAu/200GeV/physics2/picoLists/picoList_bad_MB.list
```

###How to run this code:  
```bash
# For testing we can run the code on one file:
tail -n1 fileLists/Run14/AuAu/200GeV/physics2/picoNpeLists/daily/picoNpeList_2015-05-20.list > test.list
ln -s `pwd`/auau200GeVRun14/StRoot/macros/runPicoNpeAnaMaker.C
root4star -l -b -q -x runPicoNpeAnaMaker.C\(\"test.list\",\"test.root\"\)
```

###How to submit jobs:
```bash
# You cah find STAR Scheduler XML file under:
cp -p auau200GeVRun14/starSubmit/submitPicoNpeAnaMaker.xml .
# auau200GeVRun14/starSubmit/README contains a how to use.
```
