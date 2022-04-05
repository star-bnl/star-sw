We will give you brief introductions about the macros in this directory. 
Macros in directory are used to generate pi0 tree and calculate double spin asymmetry step by step after you cvs co the pi0 finder software to your directory. 

0. Compile the pi0 finder by cons in your directory. Before cons, make sure you activate '#define MonteCarloS' for MC data. For real and pythia data, you need to switch off '#define MonteCarloS' in the StEEmcIUClusterMaker.cxx file. Furthermore, in StEEmcIUPi0Analysis.cxx file, '#define EFFICIENCY' for MC/pythia sampleto calculate reconstruction efficiency, and turn off the definition for real data(must be done).

1. runEEmcMCPi0.C
The main macro to run pi0 finder developed by IUCF star spin group.
The macro works for the MC/pythia data analyses.

2. runEEmcPi0.C
The main macro to run pi0 finder developed by IUCF star spin group.
The macro works for the real data analyses.

3. eff.C
The macro to calculate pi0 reconstruction efficiency for MC/pythia.

4. normalizemass.C
The macro normalizes the spin-dependent pi0 yield according to relative luminosity after we have generated EEMC pi0 trees according to pi0 finder software. We read these pi0 trees, initialize the luminosity for each fill and each run accordingly, and save the information we want into an output root file for further usage.

5. FitPtMass.C
This is a simple macro to test the fitting function and result interactively. The input data is a non-tree root file from pi0 tree. The default name here is 'allruns.hist.root', but users may change it by your preference.

6. FitPt2Mass.C
After we test the parameter values and fit function from FitPtMass.C, we fix some parameters and start generating four spin-depdendent pi0 yields automatically.
7. Calall.C
This macro is designed to calculate the single spin asymmetries from yellow and blue beams, and double spin asymmetry from pi0 study, based on the generated/fitted/normalized pi0 yield from FitPt2Mass.C in our fitting philosopy. 

8. BackgroundLL.C
This macro calculates the background ALL based on the side-band analysis.

9. To run macros above in your directory, simply type root4star (-b -q) *.C

10. Scheduler examples: prod.xml and prod2.xml. Schedulers are used to generate pi0 tree automatically for real data analyses. People do jobs remotely instead of retrieving MuDst data to local disk.
To run the scheduler for a certain run and input from HPSS:
star-submit-template -template prod.xml -entities run=7136076,output=/star/institutions/iucf/hew/dir/
To run a scheduler with a local disk input:
star-submit prod2.xml

11. bsub command usage example
Use bsus so that we don't do jobs interactively. Here is an example of running the normalization macro:
bsub -q star_cas_short -J job7847 -o outLog/log7847 root -q -b normalizemass.C\(-1,\"/star/institutions/iucf/hew/Histout/7847/\",\"/star/institutions/iucf/hew/Outhist/FillTest/7847.root\"\)

12. hadd command usage example
Once you have generated all pi0 trees you want and normalize them, you want to add all root files together, so that you can reach histograms conveniently. Go to the directory where you save your trees or normalized root files and type:
hadd -T test.root *.root
This line of command addes them together and gives your the result: test.root. Note: *.root is the source file. If you want to read and save them in a different directory, you can type:
hadd -T output/test.root workdir/*.root. and make sure directories output and workdir existing.

