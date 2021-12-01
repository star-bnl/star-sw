How to run Embedding 
____________________
1. Setup TFG environmnet 
    source /afs/rhic.bnl.gov/star/packages/.DEV2/setupDEV2.csh
2. Setup TFG Release. The list of TFG releases can be found /afs/rhic.bnl.gov/star/packages/.DEV2/TFG_Releases
    setup STFG18d
3. An example to run simulation and reconstruction on HLT farm
   a. ~fisyak/xml/2016Embedding.xml
   b. Set of runs defined via ~fisyak/bin/MuDstCommand.pl
   c. The chain is defined as
      Chain = "Vmc,'${name}',VMCAlignment"
    where name is the name of Cint macro in $STAR/StarDb/Generators which defines type of particle, decay mode and pT - distribution
        The list of available generators contains
                  antiprotonmTsq5PerCentZ6cm protonmTsq5PerCentZ6cm \
                  KNmTsq5PerCentZ6cm KPmTsq5PerCentZ6cm \
                  piNmTsq5PerCentZ6cm piPmTsq5PerCentZ6cm \
                  D02KmTsq D0K0s2KmTsq D0K0s2pimTsq D0K3pimTsq D0KpimTsq \
                  DK0s3pimTsq DK0spimTsq DK2pimTsq \
                  DsK0sK0spimTsq DsK0sK2pimTsq DsK0sKmTsq DsKKpimTsq \
                  Lc3pi Lc3pimT Lc3pimTsq LcKppi LcKppimT LcKppimTsq \
                  D0KpiCutpT0_1 D0KpiCutpT3_10 D0KpiCutpT0 DK2pimCutpT0 D0barKpiCutpT0 D02KmCutpT0
