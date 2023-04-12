# Run-by-run QA script developed for STAR BES-II run-by-run QA
# authors (c) C.Y. Tsang, Y. Hu, P. Tribedy 2022-07-19
# PLEASE WRITE TO <ctsang@bnl.gov>, <yuhu@bnl.gov>, <yuhu@lbl.gov>, <ptribedy@bnl.gov> for any questions on the package
# edit by Y. Hu, and C.Y. Tsang, 2022-07-21
# last edit by Y.Hu to fix 1) the issue in badrunfind.sh when calculate the RMS; 2) the plotting issue (missing last run) 2022-08-25

##############################################
############   BEFORE YOU RUN   ##############
##############################################
1. Saved the run-by-run information with TProfile in a root file. The x-axis should be the run-number/run-index. (See "qahist.root" as an example.)
2. Prepare the variable name you want to check in file, each variable take one line. (See the "QA_variable.list" as an example)
3. Install the python3 and related pacage (see INSTALLATION section)


##############################################
####   INSTALLATION (PYTHON3 & PACKAGE)   ####
##############################################
This installation takes up about 3 GB of space. You can either do an automatic or a manual installation.
***************************
Automatic installation: 
An installations script called "install.sh" is provided with the package. Simply type
	  $> bash install.sh
It will install all relevant packages for you. Occasionally it will ask for your permission to proceed. Type "y" for all of them and everything will be installed in the directory from which install.sh is called.

CAUTION: It install miniconda3 on your current directory. If you already have miniconda3 installed, make sure that you’ve initialize it before running the script to prevent it from being installed again. Also the script modifies ~/.bashrc such that miniconda is activated during start-up. 
***************************
Manual installation: 
The outlier detection script relies on packages installed on anaconda virtual environment. You need to install anaconda/miniconda and then install all the relevant python packages. Here’s are the steps:
1.     Install anaconda/miniconda by following instructions from https://www.anaconda.com/ . 
2.     (Optional) update your conda channels by $>conda config --add channels conda-forge
3.     Create an environment called “Segment” with packages “matplotlib”, “ruptures”, “pandas”, “numpy” and “scipy” by $>conda create -n Segment matplotlib ruptures pandas numpy scipy.
a.     If you don’t want to call the environment “Segment”, you need to modify runbyrun_v2.sh. The required modifications will be detailed in later sections, calling it “Segment” will cause the least troubles.

##############################################
########   RUN THE PACKAGE ON RCF    #########
##############################################
To run the pacakge, simply do $> bash runbyrun_v2.sh FILENAME [e.g qahist.root ] NAMELIST [e.g name.list ] SYSTEM [e.g Au+Au ] ENERGY [19.6]
For example:
	$> bash runbyrun_v2.sh qahist.root QA_variable.list  Au+Au 19.6


##############################################
############   OUTPUT YOU GOT    #############
##############################################
The "breakPt.txt" contains the stable run period, for example, the list: 
20058004
20060069
20062043
..
means the stable run periods are 20058004-20060068, 20060069-20062042, 20062043-...

The "badrun.list" contains the bad runs. Variable names listed after the runID marked the badruns according to which variable


##############################################
######  RUN THE PACKAGE ON LOCAL  PC  ########
##############################################
1. Install the anaconda/miniconda and create an environment called “Segment” with packages “matplotlib”, “ruptures”, “pandas”, “numpy” and “scipy” by $>conda create -n Segment matplotlib ruptures pandas numpy scipy. (See "INSTALLATION" section, "Manual installation" for more details)
2. Install the gnuplot (version older than 5.0)
3. Change the "/star/u/jdb/.exodus/bin/gnuplot <<EOF" in to "gnuplot <<EOF" in "plot_runbyrun.sh"
4. Run the same command to do the run-by-run QA: $> bash runbyrun_v2.sh FILENAME [e.g qahist.root ] NAMELIST [e.g name.list ] SYSTEM [e.g Au+Au ] ENERGY [19.6]

##############################################
##########   CHANGE THE PLOTTING     #########
##############################################
1. If you only want to plot, prepare the data file with the same formate as runInfo_runidvsavgdedx.txt. Then run: bash plot_runbyrun.sh HISTNAME [e.g runidvsavgdedx] SYSTEM [e.g Au+Au] ENERGY [19.6] NSIGMA [e.g 20], for example:
      $> bash plot_runbyrun.sh runidvsavgdedx Au+Au 19.6 20
The default NSIGMA (Y-range) is 20-RMS for the good runs
2. If you want to change the color:
Change "color1"..."color6" with whatever the color you prefer in "plot_runbyrun.sh". Color is in hex mode, see https://www.colorhexa.com/ for colors.
3. If you want to change the output file format, change '.eps' to any image format (e.g '.jpg') in "plot_runbyrun.sh"

####################################################
##########   CHANGING THE INPUT BETA     ###########
## internal segmentation identification parameter ##
####################################################
The current package has an automated searching function to search for the input parameters using the segmentation identification algorithm (input to "Segmentation2.py" file). You don't need to make any changes in principle. But the package still gives you the freedom to tune the input parameters. The current setup will loop "0.5, 1, 2, 5, 9" these 5 beta values and find one that gives you the most badruns. But you can vary the beta value if you want. 
In short, the number of segmentation and the number of bad runs are correlated. The different beta will give you a different number of segmentations, but should still give you a similar number of badruns.
