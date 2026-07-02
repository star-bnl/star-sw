#!/usr/bin/python
# -*- coding: utf-8 -*-

# PURPOSE
# Filter a text file copy of the polarization table from the RHIC polarimetry page for Run 22 into a format to be read by the analysis class #StFwdPolAna
#
# DESCRIPTION
# This is a quick and dirty program to read the polarization tables taken from (RHIC spin page)[https://wiki.bnl.gov/rhicspin/Results_(Polarimetry)]. The Run 22 result table can be found here (Run 22 Polarization results)[https://wiki.bnl.gov/rhicspin/Run_22_polarization]. You need to copy the results from that page ignoring the column headers which will be added by this program. You then you need to fill in zeros '0' into the nonexistent entries and adding the +- symbols where appropriate if they are missing. This program expects the name of that text file to be 'Run22PolFromWeb_20250205.txt' however you are free to give it any name as long as you change the input file name accordingly below. The output file produced is called 'Run22PolForJobs.txt' but can be changed accordingly below.
#
# AUTHOR
# David Kapukchyan
#
# LOG
# @[February 5, 2025] > First instance
#
# CAVEATS
# to run this script use my SciKit conda environment in the starpro environment. Or any python environment where numpy and pandas are installed

import numpy as np
import pandas as pd

#pd.set_option("display.max_rows", None)   #For printing all rows of the dataframe

#@[September 13, 2024] > [Add column names when not in file](https://sparkbyexamples.com/pandas/pandas-add-column-names-to-dataframe/)
column_names = ["Fill", "Energy", "Start", "Stop", "Blue_P0", "Blue_pm", "Blue_DP0", "Blue_dPdT", "Blue2_pm", "Blue_DdPdT","Yellow_P0", "Yellow_pm", "Yellow_DP0", "Yellow_dPdT", "Yellow2_pm", "Yellow_DdPdT"]
#Read file with space separator](https://stackoverflow.com/questions/19632075/how-to-read-file-with-space-separated-values-in-pandas)
PolList = pd.read_csv('Run22PolFromWeb_20250205.txt',sep="\s+",names=column_names)

#print(PolList)
#Drop the +- columns to clean up
PolList.drop("Blue_pm",axis=1,inplace=True)
PolList.drop("Blue2_pm",axis=1,inplace=True)
PolList.drop("Yellow_pm",axis=1,inplace=True)
PolList.drop("Yellow2_pm",axis=1,inplace=True)
#print(PolList)
#print('\n')

#Remove missing polarized data
PolList = PolList.loc[ (PolList['Blue_P0']!=0) ] #zeros added articially in the file to make removing nonexistent values easier
print( PolList )


File = open("Run22PolForJobs.txt",mode='wt',buffering=1) #The '1' at the end means only keep one line in python buffer before writing to file

#@[September 12, 2024] > [How to write dataframe to file](https://stackoverflow.com/questions/31247198/python-pandas-write-content-of-dataframe-into-text-file)
File.write( PolList.to_string(header=False,index=False) )
File.write('\n')



