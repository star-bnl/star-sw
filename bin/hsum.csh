#! /usr/local/bin/tcsh -f
root.exe -q -b H*AuAu_1_*.root 'Hadd.C("Hist890P04ig_AuAu_1.root")'
root.exe -q -b H*AuAu_2_*.root 'Hadd.C("Hist890P04ig_AuAu_2.root")'
root.exe -q -b H*AuAu_3_*.root 'Hadd.C("Hist890P04ig_AuAu_3.root")'
root.exe -q -b H*AuAu_62_*.root 'Hadd.C("Hist890P04ig_AuAu_62.root")'
root.exe -q -b H*pp_*.root 'Hadd.C("Hist890P04ig_pp.root")'
root.exe -q -b Hist890P04ig_AuAu_1.root Hist890P04ig_AuAu_2.root Hist890P04ig_AuAu_3.root Hist890P04ig_AuAu_62.root Hist890P04ig_pp.root Hadd.C
mv Hist890P04ig_AuAu_1.root Hist890P04ig_AuAu_2.root Hist890P04ig_AuAu_3.root Hist890P04ig_AuAu_62.root Hist890P04ig_pp.root Hist890P04igAuAu200.root ../../Histograms/
cd ~/.dev/
fit.pl Histograms/Hist890P04ig_AuAu_1.root 
fit.pl Histograms/Hist890P04ig_AuAu_2.root 
fit.pl Histograms/Hist890P04ig_AuAu_3.root 
fit.pl Histograms/Hist890P04ig_AuAu_62.root 
fit.pl Histograms/Hist890P04ig_pp.root 
fit.pl Histograms/Hist890P04igAuAu200.root
