How to simulate and reconstruct signal.
________________________________________
1. Setup TFG environment 
    source /afs/rhic.bnl.gov/star/packages/.DEV2/setupDEV2.csh
2. Setup TFG Release. The list of TFG releases can be found /afs/rhic.bnl.gov/star/packages/.DEV2/TFG_Releases
    setup .DEV2
3. An example to run simulation and reconstruction  
    a. ~fisyak/xml/2019VFMCE.xml 
      It is supposed that you submit the xml-file in directory with name matching one of the Cint macro with event generator
      The list of available generator names contains:
(K_S0  Lambda0_bar Lambda0 Xi_bar Xi  Omega_bar Omega AntiHyper2NeutronFixed AntiHyperH4Fixed AntiHyperNeutronFixed AntiHyperTritonFixed Hyper2NeutronFixed HyperH4Fixed HyperNeutronFixed HyperTritonFixed AntiHyper2Neutron AntiHyperH4 AntiHyperNeutron  AntiHyperTriton Hyper2Neutron HyperH4 HyperNeutron HyperTriton
    b. Set of runs defined via ~fisyak/bin/RunId.pl 
      # RunId.pl firstRun lastRun No_event_per_Run name_of_macro (unused)
	RunId.pl        1     100             1000 dummy 
     c. The chain is defined as
      Chain = "MC.2019a,P2019a,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeakFull,-evout,NoHistos,noTags,noRunco,Stx,KFVertex,picoWrite,PicoVtxVpdOrDefault,-geantOut,-evout,-useXgeom,"${name};
    where  
    -   name is the name of Cint macro in path .:./StarDb/Generators:$STAR/StarDb/Generators which defines type of particle, decay mode and pT - distribution
    - VMCE = VMCE 
    - sdt  = "" for Ideal geometry or "sdt20190311" for real one
________________________________________
   
