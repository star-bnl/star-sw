class StChain;
StChain *chain=0;
 
int rdMuWana2012(
	     int nEve=1e6,
	     char* file    = "",
	     int isMC=0, // 0=data 35X=embedding
	     int useJetFinder = 0, // 0 - no jets=badWalgo; 1 generate jet trees; 2 read jet trees 
	     int idL2BWtrg=0, //  offline Ids  needed for real data
	     int idL2EWtrg=0, // run 9 L2EW
	     // make those below  empty for scheduler 
	     char* muDir   = "",
	     char* jetDir   = "",
	     char* histDir   = "",
	     char* wtreeDir   = "",
	     int  spinSort=true,
	     bool findZ=true,
	     int  geant=false
  ) { 
  
  if(isMC==0) //jetDir="/star/institutions/iucf/stevens4/run12w/sampler-pass2-A/jets/";
  jetDir="./"; 

  //if(isMC &&  useJetFinder==2) geant=true;
  
  if(isMC) spinSort=false;
  TString outF=file;
  if(isMC==0) {
    outF=file;
    printf("Unpack file=%s=\n",file);
    char *file1=strstr(file,"/R")+1;
    printf("file1=%s=%s=\n",file1);
    outF=file1; file1=outF.Data();
    printf("OutF=%s=\n",outF.Data());
  } 
  else if(isMC < 400) {
    char *file1;
    //private 2012 MC
    if(isMC==100) file1=strstr(file,"jba"); 
    //embedding run with geant files
    if(isMC==350) file1=strstr(file,"W");
    if(isMC==351) file1=strstr(file,"Wtau");
    if(isMC==352) file1=strstr(file,"Z");
    assert(file1);  printf("file1=%s=\n",file1);
    outF=file1; 
  }
  else { // bad isMC flag
    cout<<"bad isMC flag"<<endl; return;
  }
  outF=outF.ReplaceAll(".lis","");
  outF.ReplaceAll(".MuDst.root","");
  printf("TRIG ID: L2BW=%d, L2EW=%d   isMC=%d  useJetFinder=%d\n",idL2BWtrg,idL2EWtrg,isMC, useJetFinder );

  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gROOT->LoadMacro("LoadLogger.C");
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");
  
  assert( !gSystem->Load("StDaqLib"));

  // libraries below are needed for DB interface
  assert( !gSystem->Load("StDetectorDbMaker")); // for St_tpcGasC
  assert( !gSystem->Load("StTpcDb"));
  assert( !gSystem->Load("StDbUtilities")); //for trigger simu
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StEEmcUtil"));
  assert( !gSystem->Load("StEEmcDbMaker"));
  assert( !gSystem->Load("StTriggerFilterMaker"));
  assert( !gSystem->Load("StWalgo2011"));
  assert( !gSystem->Load("StTriggerUtilities"));
  assert( !gSystem->Load("StSpinDbMaker"));
  
  if (useJetFinder ==1 || useJetFinder == 2){ // jetfinder/jetreader libraries
    cout << "BEGIN: loading jetfinder libs" << endl;
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StRandomSelector");
    gSystem->Load("libfastjet.so");
    gSystem->Load("libCDFConesPlugin.so");
    gSystem->Load("libEECambridgePlugin.so");
    gSystem->Load("libJadePlugin.so");
    gSystem->Load("libNestedDefsPlugin.so");
    gSystem->Load("libSISConePlugin.so");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StJets");
    gSystem->Load("StJetEvent");
    gSystem->Load("StJetMaker");
    gSystem->Load("StTriggerFilterMaker");
    cout << "END: loading jetfinder libs" << endl;
  }
  else  {
    cout << "\nWARN: Jet are NOT read in, W-algo will not wrk properly\n " << endl;
  }
  
  if(geant){                                  
    // libraries for access to MC record  
    assert( !gSystem->Load("StMcEvent"));      
    assert( !gSystem->Load("StMcEventMaker")); 
  }
  
  cout << " loading done " << endl;
  
  // create chain    
  chain = new StChain("StChain"); 
  // create histogram storage array  (everybody needs it):
  TObjArray* HList=new TObjArray;
  TObjArray* HListTpc=new TObjArray;

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
  int nFiles=1000;
  muMk = new StMuDstMaker(0,0,muDir,file,"MuDst.root",nFiles);
  muMk->SetStatus("*",0);
  muMk->SetStatus("MuEvent",1);
  muMk->SetStatus("EmcTow",1);
  muMk->SetStatus("EmcSmde",1);
  muMk->SetStatus("EmcSmdp",1);
  muMk->SetStatus("EEmcSmdu",1);
  muMk->SetStatus("EEmcSmdv",1);
  muMk->SetStatus("EEmcPrs",1);
  muMk->SetStatus("PrimaryVertices",1);
  muMk->SetStatus("GlobalTracks",1);
  muMk->SetStatus("PrimaryTracks",1);
  TChain* tree=muMk->chain(); assert(tree);
  int nEntries=(int) tree->GetEntries();
  printf("total eve in muDst chain =%d\n",nEntries);  // return ;
  if(nEntries<0) return;
  

  //for EEMC, need full db access:
  St_db_Maker   *dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");
  
  if (isMC==0) { 
    //data
    dbMk->SetFlavor("Wbose2","bsmdeCalib");// Willie's abs gains E-plane, run 9
    dbMk->SetFlavor("Wbose2","bsmdpCalib"); // P-plane
    //dbMk->SetFlavor("sim","bemcCalib"); // use ideal gains 
    //dbMk->SetFlavor("sim","eemcPMTcal"); // use ideal gains 
  }     
  else if (isMC<400) {
    dbMk->SetFlavor("Wbose2","bsmdeCalib");// Willie's abs gains E-plane, run 9
    dbMk->SetFlavor("Wbose2","bsmdpCalib"); // P-plane
  }
  else {
    printf("???? unforeseen MC flag, ABORT\n"); assert(1==2);
  }
  
  
  //.... load EEMC database
  StEEmcDbMaker*  mEEmcDatabase = new StEEmcDbMaker("eemcDb");

  if(geant){                                            
    StMcEventMaker *mcEventMaker = new StMcEventMaker();  
    mcEventMaker->doPrintEventInfo = false;               
    mcEventMaker->doPrintMemoryInfo = false;              
  }
  
  //.... Jet finder code ....
  if (useJetFinder > 0)  {
    TString jetFile = jetDir; jetFile+="jets_"+outF+".root";
    cout << "BEGIN: running jet finder/reader =" <<jetFile<<"="<< endl;
  }

  if (useJetFinder == 1){// run jet finder
    double pi = atan(1.0)*4.0;    
    // Makers for clusterfinding
    StSpinDbMaker* spDbMaker = new StSpinDbMaker("spinDb");
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
    
    //Instantiate the JetMaker and SkimEventMaker
    StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muMk, jetFile);
    //StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muMk,outSkimFile);
    
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
    cout<<"run "<<file<<" nEve="<<eventCounter<<" total "; tt.Print();
    printf("****************************************** \n");
    
    int t2=time(0);
    if(t2==t1) t2=t1+1;
    float tMnt=(t2-t1)/60.;
    float rate=1.*eventCounter/(t2-t1);
    printf("#sorting  done %d of   nEve= %d, CPU rate= %.1f Hz, total time %.1f minute(s) \n\n",eventCounter,nEntries,rate,tMnt);
    
    cout << "END: jet finder " << endl;
    return;
  }
  if (useJetFinder == 2)
    {
      cout << "Configure to read jet trees " << endl;
      StJetReader *jetReader = new StJetReader;
      jetReader->InitFile(jetFile);
    }
  
  
  //.... W reconstruction code ....
  St2011WMaker *WmuMk=new St2011WMaker();
  if(isMC) { // MC specific
    WmuMk->setMC(isMC); //pass "version" of MC to maker
    //WmuMk->setJetNeutScaleMC(1.0); 
    //WmuMk->setJetChrgScaleMC(1.0); 
  }else {// real data
    //WmuMk->setBtowScale(0.976); //determined from Run 9 data 
    WmuMk->setTrigID(idL2BWtrg,idL2EWtrg);
  }
  
  TString outFtree=wtreeDir; outFtree+=outF; outFtree+=".Wtree.root";
  WmuMk->setTreeName(outFtree);
  
  if (useJetFinder == 2) WmuMk->setJetTreeBranch("ConeJets12_100","ConeJets12_100_noEEMC"); //select jet tree braches used
  
  WmuMk->setMaxDisplayEve(100); // only first N events will get displayed 
  
  /* evaluation of result, has full acess to W-algo internal data
     including overwrite - be careful */
  
  WpubMk=new St2011pubWanaMaker("pubJan");
  WpubMk->attachWalgoMaker(WmuMk); 
  
  //Collect all output histograms   
  //already defined this above:  TObjArray* HList=new TObjArray;
  WmuMk->setHList(HList);
  WmuMk->setHListTpc(HListTpc);
  WpubMk->setHList(HList);
  
  StSpinDbMaker *spDb=0;
  if(spinSort){
    spDb=new StSpinDbMaker("spinDb");
    enum {mxSM=5}; // to study eta-cuts, drop Q/PT cut
    St2011pubSpinMaker *spinMkA[mxSM];
    for(int kk=0;kk<mxSM;kk++) {
      char ttx[100]; sprintf(ttx,"%cspin",'A'+kk);
      printf("add spinMaker %s %d \n",ttx,kk);
      spinMkA[kk]=new St2011pubSpinMaker(ttx); 
      spinMkA[kk]->attachWalgoMaker(WmuMk);
      spinMkA[kk]->attachSpinDb(spDb);
      spinMkA[kk]->setHList(HList); 
      if(kk==1) spinMkA[kk]->setEta(-1.,0.);
      if(kk==2) spinMkA[kk]->setEta(0,1.);
      if(kk==3) spinMkA[kk]->setQPT(-1);// disable Q/PT cut
      if(kk==4) spinMkA[kk]->setNoEEMC(); 
    }  
  }
  
  if(geant){
    pubMcMk=new St2009pubMcMaker("pubMc");
    pubMcMk->attachWalgoMaker(WmuMk);
    pubMcMk->setHList(HList);
  }
  
  if (findZ){
    ZMk=new St2011ZMaker("Z"); 
    ZMk->attachWalgoMaker(WmuMk);
    ZMk->setHList(HList); 
    ZMk->setNearEtFrac(0.88);
    ZMk->setClusterMinEt(15);
    ZMk->setPhi12Min(3.1416/2.);
    ZMk->setMinZMass(73.); // Zmass -20%
    ZMk->setMaxZMass(114.);// Zmass +20%
  }
  
  
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
  //chain->Finish();
  
  cout<<"run "<<file<<" nEve="<<eventCounter<<" total "; tt.Print();
  printf("****************************************** \n");

  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*eventCounter/(t2-t1);
  printf("#sorting %s done %d of   nEve= %d, CPU rate= %.1f Hz, total time %.1f minute(s) \n\n",file,eventCounter,nEntries,rate,tMnt);
  

  
  
  TString outFh=histDir; outFh+=outF; outFh+=".wana.hist.root";
  
  cout<<"Output histo file "<<outFh<<endl;

  hf=new TFile(outFh,"recreate");
  if(hf->IsOpen()) {
    //HList->ls();
    HList->Write();
    //write TPC histos to new directory
    TDirectory *tpc = hf->mkdir("tpc");
    tpc->cd();
    HListTpc->Write();
    printf("\n Histo saved -->%s<\n",outFh.Data());
  } else {
    printf("\n Failed to open Histo-file -->%s<, continue\n",outFh.Data());
  }
  //WmuMk->Finish();
  
  return;  
}


// $Log: rdMuWana2012.C,v $
// Revision 1.5  2012/07/05 20:13:33  balewski
// *** empty log message ***
//
// Revision 1.4  2012/06/25 20:57:05  stevens4
// add directory for tpc histos
//
// Revision 1.3  2012/06/22 18:23:36  balewski
// *** empty log message ***
//
// Revision 1.2  2012/06/18 18:32:50  stevens4
// remove hard coded jet path
//
// Revision 1.1  2011/02/10 20:33:35  balewski
// start
//
