# Run Glauber simulation

## Short summary

Step-by-step actions are listed below.

Rename directory "Makers" to "StRoot".

Then:
```bash
./prepare.sh
cons
```

Edit nEvents in all_submit_doFastGlauberMcMaker.csh. Recommended: nevents=200000
```bash
./all_submit_doFastGlauberMcMaker.csh RuRu_Case1 200 0 5 1
```

Edit creatList.csh
```bash
./creatList.csh
```

If you previously produced ncoll_npart.root, the following will replace it
```bash
root4star -b -q -l addNcollVsNpart.C
```

Change nevents, root filename, and lower multiplicity cut in doScanX_my.csh
Change hist name in doNbdFitMaker.C
Change parameters in submit_doScan.pl
Remove all previous files from RatioChi2Files/
Remove all previous files from LOG_Scan/
```bash
rm RatioChi2Files/*.root
rm LOG_Scan/*
./submit_doScan.pl
```

Wait for jobs to finish
```bash
mkdir LOG_Scan_Zr
mkdir RatioChi2Files_Zr
mv LOG_Scan/* LOG_Scan_Zr/
mv RatioChi2Files/*.root RatioChi2Files_Zr/
```
or
```bash
mkdir LOG_Scan_Ru
mkdir RatioChi2Files_Ru
mv LOG_Scan/* LOG_Scan_Ru/
mv RatioChi2Files/*.root RatioChi2Files_Ru/
```
Then (only once you've scanned both Ru and Zr)
```bash
cd getBestChi2_fromCat
./extractChi2LinesForIsobars.sh
```

Edit nevents in genMinChi2ListForIsobars.sh: Recommended 2000000
```bash
./genMinChi2ListForIsobars.sh
```

### Step 0. Prerequisite

Rename directory "Makers" to "StRoot". Currently, it is named Makers in
order not to confuse autobuild (CI/CD) in git.

### Step 1. Initialization

```bash
./prepare.sh
```
To create needed directories, please look into this simple script before run it, 
just in case of deleting your important files by mistake.
A tip here, put all the codes under your RCF home directory and set the outDir to be on your PWG disk.

Then execute “cons”, (to compile all the codes, please be sure there is no errors to compile the codes)
```bash
cons
```

```bash
./all_submit_doFastGlauberMcMaker.csh AuAu 27 0 5 1
```
If you want to generate deformed nuclei, set the default deformation to kTRUE in submit_glauber.pl
The first input parameter:  27 is the energy per nucleon
The second input parameter: 0 is the start job index
The third input parameter:  5 is the end job index
The fourth input parameter: 1 means "submit jobs”;  0 means “only test, won't submit"

Make nEvents one fifth of the total number you want. If you want 1E6, set nevents=2E5
For the test run, you can change the “nevents” in all_submit_doFastGlauberMcMaker.csh to 10, 
to submit a test run, after all jobs finish successfully and the output files looks good 
(if you don’t know they are good or not, just move forward, you will know it later), then change 
it to the 100k (100000) or 500k, for real submissions.

### Step 2. NPD and finding the best fit parameters

After the previous jobs finished, then you can Scan Negative Binomial Distribution (NBD) fitting parameters for the best parameters
```bash
./creatList.csh
```
EDIT this before running. This script is to prepare the input TTree file list.

```bash
root4star -b -q -l addNcollVsNpart.C
```
Create ncoll_npart.root file in the current directory, this will be used for the NBD scan 
on the parameters: npp, k, x, and eff. The scan ranges of npp, k, x and eff can be defined
in “submit_doScan.pl”, in order to set a reasonable ranges, 
you better check the best parameters from previous study first, the previous parameters can be found 
in “StRoot/StCentralityMaker/StCentrality.cxx”, if you only want to submit 1 job with a constant npp, k, x 
and eff values, you can set nppbin, kin, xbin, effbin =1 and let the min and max of npp, k, x and eff be same value. 
Please note that, the goal of scan is to find the best parameters matching to real data, the real data here is 
the Refmult distribution after all corrections (luminosity, vertex…),  usually called RefmultCorr. In this example, 
you can find the Refmult is saved in a root file “ run18_27Gev_MB_refMultCorr.root” , the corresponding histogram is 
named as “hRefMultCorr”, the name of the root file need to be given in the “ doScanX_my.csh”, and the name of the 
histogram need to be given in “doNbdFitMaker.C”. Please make sure you have the input root file under this directory.

Another thing need to be noted is the Refmult lower limit cut in doScanX_my.csh, usually cut on Refmult>100 for 
the 200 GeV AuAu Refmult scan, for 27 GeV, we choose to cut on 50. This cut is the fitting Refmult fitting range 
(while compare MC to Data).

After you selected the parameter ranges, put the root file with hRefmultCorr under this directory, set the Refmult Cut and modify 
the names in the script and code as above, you can then start to submit jobs for the scan by “./submit_doScan.pl"

After the above jobs finished, you will find the output root files under “RatioCHi2Files” which include “chi2*.root” and “Ratio*.root”, 
they are named by the events, npp, k, x and eff values. The out log files corresponding these jobs can be found at LOG_Scan. 

Then you can “cd getBestChi2_fromCat”, run the following scripts to find the best Chi2/ndf root files, and then based on the names of the 
root file, you will know all the parameter values.

```bash
./extractChi2Lines.sh 
```
It will read the output log files and write down the lines with Chi2/ndf information 

```bash
./genMinChi2List.sh 
```
Adjust nevents! This script will read the output from previous step and find the least Chi2/ndf root files and write them into 
a file “minChi2_file.list”, please note that, here we still don’t know the eff values corresponding to the least Chi2/ndf, 
we need one more step

Run findMinChi2.C in root:
```bash
root -l
> .L findMinChi2.C
> findMinChi2()
```

```bash
./getBestChi2RootFile.sh
```
It will finally find best parameters of npp,k,x,eff and the root files corresponding to the least Chi2/ndf 
and copy them into “bestChisRootFile".

After obtained all the parameter values, and the corresponding output root files, we can get the Ratio of MC/Data in 
the root file (“Ratio_npp1.270_k1.800_x0.160_eff0.110.root”), named as “hRatio”, you can then use this ratio to 
weight your RefmultCorr in the real data, and obtain the RefmultCorr_Weighted and save into a new root file, 
in my case I name the root file as “run18_27Gev_MB_refMultCorr_Weighted.root”, you can then fix the fitting 
parameters in “./submit_doScan.pl” and modify the input root file name in “doScanX_my.csh” and input histogram 
name in “doNbdFitMaker.C”, and then submit it a single job by “./submit_doScan.pl” to get a new “hRatio” histogram 
in the Ratio……root file. You should find that the new “hRatio” should be flat at 1, since the the new input 
“hRefmultCorr_Weighted” is obtained by weighting the RefmultCorr with MC/Data ratio. It is always good to make 
a plot to compare the hRefmultCorr, hRefmultsim, hRatio before and after reweitht, to make sure everything working correctly.

If everything so far looks good, then you finished most parts of the steps. Now move to the systematic uncertainty calculaltion in the next step.

### Step 3. Systematic Uncertainty Estimation

Go the LOG_Scan, and find the output log files like “doScanX_my_1_27_1_8_0_16_0_11.out”, the name is defined based on the 
parameter values, so it should be easy to find it.  Copy the line 173-204 and paste them to 
“StRoot/StCentralityMaker/StCentrality.cxx” and change all the npp, k, x and eff values to those you obtained in the same code.

```bash
./createList.csh
```

```bash
./all_doAnalysisMaker.csh AuAu_27GeV kFALSE kFALSE
```
In this step it will read the output tree files generated at the first step from “all_submit_doFastGlauberMcMaker.csh”,
Write down the new output files “ana*.root"

Produce all tables under ./table
```bash
./all_doPlotMaker.csh 27
```
This will make all the plots and save into ./figure
