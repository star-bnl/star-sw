/* To generate jetTree for a new M-C using BFC w/ EEss, read Mu & write jet-tree locally:
root4star -b -q 'rdMuWana.C(2000,"","rck10012_1_2000evts.MuDst.root",1,200,1)'

To run W-reco w/  new M-C files, jet read locally:
root4star -b -q 'rdMuWana.C(2000,"","rck10012_1_2000evts.MuDst.root",1,200,2)'

Jan: physic files, jet read:
root4star -b -q 'rdMuWana.C(2e3,"","/star/u/balewski/2009-Wana-pp500/fillListPhys/F10535/R10102105a_230531_230601.lis",5000,0,2)'

*/



class StChain;
StChain *chain=0;
int useEtow=3;// 0=don't use; 1=only in event-display, 2=in away sum,3=in away&near sum

int  spinSort=true;
bool isJanWjj=false; 
int  isJustin=false;
bool isRoss=true; 
int  geant=false;
 
int rdMuWana(
	     int nEve=2e3,
	     char* inDir   = "",// make it empty for scheduler 
	     char* file    = "/star/u/balewski/2009-Wana-pp500/fillListD/F10535/R10102105_230531_230601.lis",
	     int nFiles  = 5000, // max # of muDst files
	     int isMC=0, // 0=run9-data, 1=Weve, 2=QCDeve, 3=Zeve, 20=rcf10010,... 26=rcf10016
	     int useJetFinder = 2 // 0 - no jets=badWalgo; 1 generate jet trees; 2 read jet trees 
 ) { 
char *eemcSetupPath="/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/";

  TString jetTreeDir = "/star/institutions/mit/balewski/2009-pp500-jets/4.15.2010/"; //default location of jet trees for run 9 data

  if(isJanWjj){ // Jan: use isMC=100 for interactive M-C,
    if(isMC==0) jetTreeDir = "/star/institutions/mit/balewski/2009-pp500-jets/4.16.2010phys/"; 
  }
  if(isJanWjj && isMC &&  useJetFinder==2) geant=true;


  if(isMC==100||isMC==200 )  jetTreeDir="./";
  if(isMC==104)   jetTreeDir ="/star/data05/scratch/balewski/2010-Wsimu-a3/jetR04/";
  if(isMC==105)  jetTreeDir="/star/data01/pwg/balewski/2010-Wsimu-setC/jetR04split05/";
  if(isMC==107)  jetTreeDir="/star/data01/pwg/balewski/2010-Wsimu-setC/jetR07/";

  
  //submit via scheduler 
  if(isMC==30) geant=true; //uses geant files
  
  if(isMC) spinSort=false;

  TString outF=file;
  int runNo=0,bht3ID=0,l2wID=0,fillNo=0;
  if(isMC==0) {//real data: runNum, trigID, L2ID from list name
    printf("Unpack file=%s=\n",file); 
    char *file1=strstr(file,"/R")+1;
    printf("file1=%s=%s=\n",file1);
    fillNo=atoi(file1-6);
    outF=file1; file1=outF.Data();
    char *p1=strstr(file1,"_");
    char *p2=strstr(p1+1,"_");
    runNo=atoi(file1+1);
    bht3ID=atoi(p1+1);
    l2wID=atoi(p2+1);
    p1[0]=0;
    outF=file1;
    outF=outF;
    printf("OutF=%s=\n",outF.Data());
  } 
  else { //  new  MC w/ working time stamp
    char *file1=strstr(file,"cn100"); assert(file1);
    file1--;
    printf("file1=%s=\n",file1);
    outF=file1; outF.ReplaceAll(".MuDst.root","");
    TString fileG=file; fileG.ReplaceAll("MuDst","geant");
  }
  printf("TRIG ID: bht3=%d l2w=%d isMC=%d\n",bht3ID,l2wID,isMC);
  
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gROOT->LoadMacro("LoadLogger.C");
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");
  
  assert( !gSystem->Load("StDaqLib"));

  // libraries below are needed for DB interface
  assert( !gSystem->Load("StDetectorDbMaker")); // for St_tpcGasC
  assert( !gSystem->Load("StDbUtilities")); //for trigger simu
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StEEmcUtil"));
  assert( !gSystem->Load("StEEmcDbMaker"));
  assert( !gSystem->Load("StTriggerFilterMaker"));
  assert( !gSystem->Load("StWalgoB2009"));

  assert( !gSystem->Load("StTriggerUtilities"));


  if(spinSort)  assert( !gSystem->Load("StSpinDbMaker"));
  
  if (useJetFinder ==1 || useJetFinder == 2){ // jetfinder/jetreader libraries
    cout << "BEGIN: loading jetfinder libs" << endl;
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StJets");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StRandomSelector");
    gSystem->Load("StJetEvent");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");
    cout << "END: loading jetfinder libs" << endl;
  }
  else  {
    cout << "\nWARN: Jet are NOT read in, W-algo will not wrk properly\n " << endl;
  }

  if(geant){                                  
    // libraries for access to MC record  
    assert( !gSystem->Load("StMcEvent"));      
    assert( !gSystem->Load("StMcEventMaker")); 
  
    // libraries for trigger simulator
    assert( !gSystem->Load("StEmcSimulatorMaker"));
    assert( !gSystem->Load("StEEmcSimulatorMaker"));
    assert( !gSystem->Load("StEpcMaker"));
  }
  
  cout << " loading done " << endl;

  // create chain    
  chain = new StChain("StChain"); 
  // create histogram storage array  (everybody needs it):
  TObjArray* HList=new TObjArray;

  if(geant){                          
    // get geant file                    
    StIOMaker* ioMaker = new StIOMaker();
    printf("\n %s \n\n",fileG.Data());   
    ioMaker->SetFile(fileG.Data());      
    
    ioMaker->SetIOMode("r");             
    ioMaker->SetBranch("*",0,"1");   //deactivate all branches 
    ioMaker->SetBranch("geantBranch",0,"r");//activate geant Branch
    ioMaker->SetBranch("minimcBranch",0,"r");//activate geant Branch 
  }

  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  muMk->SetStatus("*",0);
  muMk->SetStatus("MuEvent",1);
  muMk->SetStatus("EmcTow",1);
  muMk->SetStatus("EmcSmde",1);
  muMk->SetStatus("EmcSmdp",1);
  muMk->SetStatus("PrimaryVertices",1);
  muMk->SetStatus("GlobalTracks",1);
  muMk->SetStatus("PrimaryTracks",1);

  //St_db_Maker   *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
  //for EEMC, need full db access:
  St_db_Maker   *dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");

  if (isMC==0) { // run 9 data
    dbMk->SetFlavor("Wbose","bsmdeCalib"); // Willie's relative gains E-plane
    dbMk->SetFlavor("Wbose","bsmdpCalib"); // P-plane
    dbMk->SetFlavor("missetTCD","eemcPMTcal");  // ETOW gains , not-standard
    dbMk->SetFlavor("sim","bemcCalib"); // use ideal gains for 2009 real data as well
  }   
  else if(isMC>=100 && isMC<200) { // to be used w/ M-C generated before EEss was added to BFC
    dbMk->SetMaxEntryTime(20100420,0); // keep the same DB snap-shot as used in BFC
    dbMk->SetFlavor("sim","eemcPMTped"); // to compensate action of fast simu
    dbMk->SetFlavor("sim","eemcPMTcal"); // to compensate action of fast simu
  }   else if(isMC>=200 ) {  // new M-C w/ EEss in BFC
    dbMk->SetMaxEntryTime(20100420,0); // keep the same DB snap-shot as used in BFC
  } else {
    printf("???? unforeseen MC flag, ABORT\n"); assert(1==2);
  }


  //.... load EEMC database
  StEEmcDbMaker*  mEEmcDatabase = new StEEmcDbMaker("eemcDb");
  
  if(!isMC && strstr(file,"fillListPhys")) {
    StTriggerFilterMaker *filterMaker = new StTriggerFilterMaker;
    filterMaker->addTrigger(230420); // AJP
    filterMaker->addTrigger(230411); // JP2
    filterMaker->addTrigger(bht3ID); // regular W -> e+ analysis
  }


  if(geant){                                            
    StMcEventMaker *mcEventMaker = new StMcEventMaker();  
    mcEventMaker->doPrintEventInfo = false;               
    mcEventMaker->doPrintMemoryInfo = false;              
  }
  
  if(geant && useJetFinder!=1){ //only use trigger simulator in W algo
    //BEMC simulator:
    StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
    emcSim->setCalibSpread(kBarrelEmcTowerId,0.15);//spread gains by 15%
    StEmcADCtoEMaker *bemcAdc = new StEmcADCtoEMaker();//for real data this sets calibration and status

    //EEMC simulator:
    new StEEmcDbMaker("eemcDb");
    StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
    //slowSim->setSamplingFraction(0.0384); // effectively scales all Tower energies with a factor of 1.3 (for old private filtered simu only!)
    slowSim->setAddPed(true);
    slowSim->setSmearPed(true);

    //Get TriggerMaker
    StTriggerSimuMaker *simuTrig = new StTriggerSimuMaker("StarTrigSimu");
    simuTrig->setHList(HList);
    simuTrig->setMC(isMC); // must be before individual detectors, to be passed
    simuTrig->useBbc();
    simuTrig->useEemc(0);//default=0:just process ADC, 1,2:comp w/trgData,see .
    simuTrig->eemc->setSetupPath(eemcSetupPath);
    simuTrig->useBemc();
    simuTrig->bemc->setConfig(2);
  }
  
  //.... Jet finder code ....
  if (useJetFinder > 0)  {
    TString outFile = "jets_"+outF+".root";
    cout << "BEGIN: running jet finder/reader =" <<outFile<<"="<< endl;
  }

  if (useJetFinder == 1){// run jet finder
    double pi = atan(1.0)*4.0;    
    // Makers for clusterfinding
    //  StSpinDbMaker* spDbMaker = new StSpinDbMaker("spinDb");
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    //here we also tag whether or not to do the swap:
    bool doTowerSwapFix = true;
    bool use2003TowerCuts = false;
    bool use2006TowerCuts = true;
    //4p maker using 100% tower energy correction
    StBET4pMaker* bet4pMakerFrac100 = new StBET4pMaker("BET4pMakerFrac100",muMk,doTowerSwapFix,new StjTowerEnergyCorrectionForTracksFraction(1.0));
    bet4pMakerFrac100->setUse2003Cuts(use2003TowerCuts);
    bet4pMakerFrac100->setUseEndcap(true);
    bet4pMakerFrac100->setUse2006Cuts(use2006TowerCuts);
    //4p maker using 100% tower energy correction (no endcap)
    StBET4pMaker* bet4pMakerFrac100_noEEMC = new StBET4pMaker("BET4pMakerFrac100_noEEMC",muMk,doTowerSwapFix,new StjTowerEnergyCorrectionForTracksFraction(1.0));
    bet4pMakerFrac100_noEEMC->setUse2003Cuts(use2003TowerCuts);
    bet4pMakerFrac100_noEEMC->setUseEndcap(false);
    bet4pMakerFrac100_noEEMC->setUse2006Cuts(use2006TowerCuts);

    //Instantiate the JetMaker
    StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muMk, outFile);

    //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
    StppAnaPars* anapars = new StppAnaPars();
    anapars->setFlagMin(0); //track->flag() > 0
    anapars->setNhits(12); //track->nHitsFit()>12
    anapars->setCutPtMin(0.2); //track->pt() > 0.2
    anapars->setAbsEtaMax(2.0); //abs(track->eta())<1.6
    anapars->setJetPtMin(3.5);
    anapars->setJetEtaMax(100.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);

    //Setup the cone finder (See StJetFinder/StConeJetFinder.h -> class StConePars)
    StConePars* cpars = new StConePars();
    cpars->setGridSpacing(105, -3.0, 3.0, 120, -pi, pi);  //include EEMC
    cpars->setConeRadius(0.7); // default=0.7
    cpars->setSeedEtMin(0.5);
    cpars->setAssocEtMin(0.1);
    cpars->setSplitFraction(0.5); //default=0.5. if 0.3 less split? 
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);

    cpars->setDebug(false);

    emcJetMaker->addAnalyzer(anapars, cpars, bet4pMakerFrac100, "ConeJets12_100"); //100% subtraction     
    emcJetMaker->addAnalyzer(anapars, cpars, bet4pMakerFrac100_noEEMC, "ConeJets12_100_noEEMC"); //100% subtraction (no Endcap)


    TChain* tree=muMk->chain(); assert(tree);
    int nEntries=(int) tree->GetEntries();
    printf("total eve in muDst chain =%d for run=%d\n",nEntries,runNo);  // return ;
    if(nEntries<0) return;

    chain->Init();
    chain->ls(3);

    char txt[1000];
    //---------------------------------------------------
    int eventCounter=0;
    int t1=time(0);
    TStopwatch tt;
    for (Int_t iev=0;iev<nEntries; iev++) {
      if(eventCounter>=nEve) break;
      chain->Clear();
      int stat = chain->Make();
       if(stat != kStOk && stat != kStSkip) break; // EOF or input error
       eventCounter++;
    }
    cout<<"run R"<<runNo<<" nEve="<<eventCounter<<" total "; tt.Print();
    printf("****************************************** \n");

    int t2=time(0);
    if(t2==t1) t2=t1+1;
    float tMnt=(t2-t1)/60.;
    float rate=1.*eventCounter/(t2-t1);
    printf("#sorting R%d done %d of   nEve= %d, CPU rate= %.1f Hz, total time %.1f minute(s) \n\n",runNo,eventCounter,nEntries,rate,tMnt);

    cout << "END: jet finder " << endl;
    return;
  }
  if (useJetFinder == 2)
  {
    cout << "Configure to read jet trees " << endl;
    StJetReader *jetReader = new StJetReader;
    jetReader->InitFile(jetTreeDir+outFile);
  }


  //.... W reconstruction code ....
  St2009WMaker *WmuMk=new St2009WMaker();
  if(isMC) { // MC specific
    WmuMk->setMC(isMC); //pass "version" of MC to maker
    //vary energy scales for syst. uncert. calc., remove it next time , also from .h,.cxx
    //WmuMk->setJetNeutScaleMC(1.0); 
    //WmuMk->setJetChrgScaleMC(1.0); 
    
  }else {// real data specific
    WmuMk->setTrigID(bht3ID,l2wID,runNo);
  }
  
  if (useJetFinder == 2) WmuMk->setJetTreeBranch("ConeJets12_100","ConeJets12_100_noEEMC"); //select jet tree braches used
  
  
  WmuMk->setMaxDisplayEve(10); // only first N events will get displayed 
  WmuMk->useEtow(useEtow);// 0=don't use; 1=only in event-display, 2=in away sum,3=in away&near sum

  //set energy scale (works for data and MC - be careful!)
  //WmuMk->setBtowScale(1.0);     
  //WmuMk->setEtowScale(1.0);

  /* evaluation of result, has full acess to W-algo internal data
     including overwrite - be careful */
  
  WpubMk=new St2009pubWanaMaker("pubJan"); // remove it next time??
  WpubMk->attachWalgoMaker(WmuMk); // 

  //Collect all output histograms   
  //already defined this above:  TObjArray* HList=new TObjArray;
  WmuMk->setHList(HList);
  WpubMk->setHList(HList);

  StSpinDbMaker *spDb=0;
  if(spinSort){
    spDb=new StSpinDbMaker("spinDb");
    enum {mxSM=5}; // to study eta-cuts, drop Q/PT cut
    St2009pubSpinMaker *spinMkA[mxSM];
    for(int kk=0;kk<mxSM;kk++) {
      char ttx[100]; sprintf(ttx,"%cspin",'A'+kk);
      printf("add spinMaker %s %d \n",ttx,kk);
      spinMkA[kk]=new St2009pubSpinMaker(ttx); 
      spinMkA[kk]->attachWalgoMaker(WmuMk);
      spinMkA[kk]->attachSpinDb(spDb);
      spinMkA[kk]->setHList(HList); 
      if(kk==1) spinMkA[kk]->setEta(-1.,0.);
      if(kk==2) spinMkA[kk]->setEta(0,1.);
      if(kk==3) spinMkA[kk]->setQPT(-1);// disable Q/PT cut
      if(kk==4) spinMkA[kk]->setNoEEMC(); 
    }  
  }
  
  if(geant && !isJanWjj){
    pubMcMk=new St2009pubMcMaker("pubMc");
    pubMcMk->attachWalgoMaker(WmuMk);
    pubMcMk->setHList(HList);
  }
    
  // Justin's code
  if(isJustin){
    pubJSMk=new St2009pubJSMaker("pubJS");
    pubJSMk->attachWalgoMaker(WmuMk); 
    pubJSMk->setHList(HList); 
  }

  if (isRoss){
    ZMk=new St2009ZMaker("Z"); 
    ZMk->attachWalgoMaker(WmuMk);
    ZMk->setHList(HList); 
    ZMk->setNearEtFrac(0.88);
    ZMk->setClusterMinEt(15);
    ZMk->setPhi12Min(3.1416/2.);
    ZMk->setMinZMass(73.); // Zmass -20%
    ZMk->setMaxZMass(114.);// Zmass +20%
  }

  if (isJanWjj){
    StMcJetCalibMaker *jetcMk;
    St2009WjjMaker * wjjMk;
    char ttx[100]; 
    for(int kk=0;kk<2;kk++) {// to study various calibration schemes
      sprintf(ttx,"%cWjj",'A'+kk);
      printf("add Wjj-maker %s %d \n",ttx,kk);      
      wjjMk=new St2009WjjMaker(ttx);
      wjjMk->attachWalgoMaker(WmuMk);
      wjjMk->setHList(HList);
      wjjMk->setSpinSort(spinSort);
      wjjMk->attachSpinDb(spDb);
      wjjMk->setMC(isMC);
      if(kk==1) wjjMk->setCorrection("/star/u/balewski/2009-Wana-pp500/jesCalib/pp500ver3.jesCorr.root"); 
      if(geant) {	
	sprintf(ttx,"%ccalJ",'A'+kk);
	printf("add jetCalMaker %s %d \n",ttx,kk);
	jetcMk=new  StMcJetCalibMaker(ttx); 
	jetcMk->setHList(HList);
	jetcMk->attachWalgoMaker(WmuMk);
	jetcMk->attachWjjMaker(wjjMk);
	jetcMk->setCorrection(kk); // 0=no corrections,
      }// end of geant
      
    }// loop over kk
  }

  TChain* tree=muMk->chain(); assert(tree);
  int nEntries=(int) tree->GetEntries();
  printf("total eve in muDst chain =%d\n",nEntries);  // return ;
  if(nEntries<0) return;

  chain->Init();
  chain->ls(3);

  char txt[1000];
  //---------------------------------------------------
  int eventCounter=0;
  int t1=time(0);  
  TStopwatch tt;
  for (Int_t iev=0;iev<nEntries; iev++) {
    if(eventCounter>=nEve) break;
    chain->Clear();
    int stat = chain->Make();
     if(stat != kStOk && stat != kStSkip) break; // EOF or input error
    eventCounter++;    
  }

  cout<<"run R"<<runNo<<" nEve="<<eventCounter<<" total "; tt.Print();
  printf("****************************************** \n");

  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*eventCounter/(t2-t1);
  printf("#sorting R%d done %d of   nEve= %d, CPU rate= %.1f Hz, total time %.1f minute(s) \n\n",runNo,eventCounter,nEntries,rate,tMnt);


  TString outFh=outF+".wana.hist.root";

  cout<<"Output histo file "<<outFh<<endl;
  hf=new TFile(outFh,"recreate");
  if(hf->IsOpen()) {
    //HList->ls();
    HList->Write();
    printf("\n Histo saved -->%s<\n",outFh.Data());
  } else {
    printf("\n Failed to open Histo-file -->%s<, continue\n",outFh.Data());
  }

  return;  
}


// $Log: rdMuWana.C,v $
// Revision 1.41  2010/08/13 16:29:07  balewski
// *** empty log message ***
//
// Revision 1.40  2010/06/30 19:00:19  rcorliss
// passes_L0() now works for simulation, using trigger simu in new macro
//
// Revision 1.39  2010/05/26 18:14:34  balewski
// make spin sorting the default
//
// Revision 1.38  2010/05/25 14:42:57  stevens4
// add new library for jet maker (StRandomSelector)
//
// Revision 1.37  2010/05/04 12:14:36  balewski
// runs now w/o jet tree
//
// Revision 1.36  2010/05/01 01:31:49  balewski
// added W->JJ code & JES calibration
//
// Revision 1.34  2010/04/16 16:07:13  balewski
// *** empty log message ***
//
// Revision 1.33  2010/04/16 00:54:01  balewski
// shoudl work w/ data & new M-C
//
// Revision 1.32  2010/04/15 21:15:11  balewski
// *** empty log message ***
//
// Revision 1.31  2010/04/15 18:22:07  balewski
// *** empty log message ***
//
// Revision 1.30  2010/04/14 22:52:56  balewski
// *** empty log message ***
//
// Revision 1.28  2010/04/14 20:00:39  balewski
// *** empty log message ***
//
// Revision 1.27  2010/04/08 23:46:21  balewski
// *** empty log message ***
//
// Revision 1.26  2010/04/06 01:50:52  stevens4
// update path to jet trees with DCA cut fix
//
// Revision 1.25  2010/03/20 19:19:07  balewski
//
// added ability to drop Q/PT cut for spin analysis
//
// Revision 1.24  2010/03/14 22:50:34  balewski
// *** empty log message ***
//
// Revision 1.23  2010/02/18 22:35:16  stevens4
// add tpc effic study and allow energy scaling for data and MC
//
// Revision 1.22  2010/02/04 03:48:55  balewski
// *** empty log message ***
//
// Revision 1.21  2010/02/04 03:48:25  balewski
// add ET for lumi monitor
//
// Revision 1.20  2010/01/28 20:10:08  balewski
// added eta dependent spin sorting
//
// Revision 1.19  2010/01/27 22:12:26  balewski
// spin code matched to x-section code
//
// Revision 1.18  2010/01/26 12:02:38  stevens4
// load proper gain tables for official rcf mc
//
// Revision 1.16  2010/01/22 20:20:18  balewski
// partialy addopted to handle new offial MC, time stamp is hardcoded
//
// Revision 1.15  2010/01/21 00:15:30  balewski
// added sector & run  dependent TPC cuts on Rin, Rout
//
// Revision 1.14  2010/01/18 03:26:21  balewski
// expanded TPC track filtering, not finished
//
// Revision 1.13  2010/01/10 03:01:39  balewski
// cleanup & nicer histos
//
// Revision 1.12  2010/01/09 00:07:36  stevens4
// add jet finder
//
// Revision 1.11  2010/01/06 14:11:17  balewski
// one Z-plot added
//
// Revision 1.10  2010/01/06 05:21:59  balewski
// cleanup
//
// Revision 1.9  2010/01/06 04:22:18  balewski
// added Q/PT plot for Zs, more cleanup
//
// Revision 1.8  2010/01/05 03:23:02  balewski
// change logic for filling btow status tables, added printout to Z-code
//
// Revision 1.7  2010/01/04 05:12:02  balewski
// added 4x4 cut to Z-algo, cleanup
//
// Revision 1.6  2010/01/03 04:38:27  balewski
// reorganized Z-algo
//
// Revision 1.5  2009/12/30 18:37:08  balewski
// code tagged in the form close to that used for the Fall 2009 DNP preliminary Jacobian peak
//
// Revision 1.4  2009/12/08 17:00:06  balewski
// *** empty log message ***
//
// Revision 1.3  2009/12/08 16:53:01  balewski
// *** empty log message ***
//
// Revision 1.2  2009/12/07 20:57:48  rcorliss
// Updated rdMuWana macro to include Z finder and use current directory for the test files hardcoded in.
//
// Revision 1.1  2009/11/23 23:00:20  balewski
// code moved spin-pool
//
