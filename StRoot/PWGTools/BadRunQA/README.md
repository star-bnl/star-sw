## Introduction

This software is designed to segment time series data points using a change point detection algorithm. The algorithm detects changes in the mean and variance of the data points and segments the data into different segments based on these changes. The software then removes outliers in each segment by removing points that are beyond a certain factor (default is 5) standard deviations from the mean of each segment. 
The code will iteration on both its change point detection and outlier rejection until no more outliers are discovered.

This code works on both Linux and Windows. It should work in Mac but it has not been tested.

## Installation

To install the software, follow these steps:
The EASY way (Linux with python version >= 3.11 only. For Windows, Mac or Linux with older python, use the recommended way):
1. Run the following command: `pip3 install -r requirements.txt`

The RECOMMENDED way (works across platforms):
1.      Install miniconda from https://docs.conda.io/en/latest/miniconda.html
2.      Create d dedicated environment using environment.yml called "QA":

`conda env create -n "QA" --file enviornment.yml`

That should be it. IF it doesn't work, it means the recommended package versions doesn't work in your machine. Do the following 3 steps instead,

2.      Create a dedicated environment by running the following command to create an environment called “QA”: 

`conda config --add channels conda-forge`

`conda create -n QA matplotlib ruptures pandas numpy scipy uproot scikit-learn`

3.      Activate the environment by running `conda activate QA`
4.      Install the remining packages with `pip install pyfiglet`


The environment is called “QA” in the example, but you can call it whatever you want. You need to activate the environment EVERY TIME you restart your terminal if you use the recommended way by

`conda activate QA`

If you are using "conda" or any other virtual environment, you can replace all "python3" with just "python" in the following sections. I just want user with older OS to be sure that they are using python3 instead of python2. 

## Basic Usage

To use the software, follow these steps:

1.      Prepare a ROOT file with one or more TProfiles in it. 
2.      (Optional, skip if you use all TProfiles in the ROOT file) Prepare a text file with names of TProfiles you want to use. Use line breaks to separate the names.
3.      Run the following command `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list`

QA plots will appear for review. Close all the windows and it will generate `badrun.list`. It shows outlier runs and the reasons for their rejections.

An example ROOT file and TProfile name file are provided in this repository. Try to run it with `python3 QA.py -i qahist.root -v QA_variable.list -o badrun.list`.

**When in doubt, run `python3 QA.py -h` to show all the available options.**

## Detailed Usage

### You want it to return rejected runs in STAR run ID.

If your low bin edges of TProfile do not correspond to STAR run ID (i.e. run ID that you can lookup on shift Log), you can supply a ID mapping file. It is a text file with a list of STAR run ID, each separated by line break, order of which aligns with bin number of runs in TProfile (i.e. first row of the text file is the STAR run ID for TProfile bin 1, second row for bin 2, etc.)

Run the following command `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list `**`-m <mapping filename>`**

### A point looks weird. I zoomed in from the interactive plot but I cannot read the run ID.

Run the following command `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list `**`--allRunID`**

It forces ALL run ID to be labelled. It looks terrible from afar, especially if you have hundreds of runs, but when you zoom in you should be able to read the run ID. NOT recommended for presentation purposes, but useful for debugging. 
 
### Too many segments. Hide run ID for segment edges.

Run the following command `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list `**`--hideEdgeRuns`**


### You do not want the QA plots to pop up.

You can run it on batch mode with `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list `**`--batch`**

### You want to save the QA plots.

Run the following command `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list `**`--genPDF`**

All QA plots will be saved as \<name of TProfile\>.pdf.

### You want to see the QA plots with all the rejected points removed.

Run the following command `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list --genPDF`**`-pg`**

All QA plots without the rejected points will be saved as \<name of TProfile\>_good.pdf.

### You want to change the rejection criteria.

The default is +/- 5 SD from mean. To change the factor of SD, run `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list`**`-rr <factor of SD>`**

### You want to change the weighting scheme in the calculation of SD and mean in each segment.

The default is to weigh each run by their number of entries. To change this behavior, run `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list`**`-w <weighting scheme>`**

\<weighting scheme\> can only be "None" (each run is weighted equally), "invErr" (inverse of uncertainty) and "entries" (default). Check `python3 QA.py -h` when in doubt.

### You modified badrun.list manually and you want to see how QA plots look like.

"plotRejection.py" is the QA plotting routine. "QA.py" imports plotting routine from that file, but "plotRejection.py" can be used stand alone. To do this, run `python3 plotRejection.py -i <ROOT filename> -v <Text file> -br badrun.list` 

Check `python3 plotRejection.py -h` when in doubt.

### You want to get rid of all the "??" in the upper left hand side of the QA plots.

You need to tell it what's the reaction system and beam energy. Run with `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list `**`-e Au+Au -s 9.8`** (assuming you are doing QA on Au + Au at sqrt(s_NN) = 9.8 GeV)

### You only want global rejection on some observables.

Prepare a text file with the names of the TProfiles where global rejection is needed. Run `python3 QA.py -i <ROOT filename> -v <Text file> -o badrun.list -gv <Global Rejection Txt file>`

