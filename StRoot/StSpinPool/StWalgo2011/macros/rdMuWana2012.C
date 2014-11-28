// example by hand, run 11: root4star -b -q 'rdMuWana2012.C(500000,"./R12099030.lis",0,2,330801,330851,"","/star/institutions/iucf/stevens4/run12w/ana/run11long/jets/")'

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
  
  if(isMC==0) jetDir="./"; 

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
    if(isMC==100) file1=strstr(file,"j");   
    //embedding run with geant files
    if(isMC==350) file1=strstr(file,"W");
    if(isMC==351) file1=strstr(file,"Wminus_tau");
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

  // libraries below are needed for DB interface and trigger
  assert( !gSystem->Load("StDetectorDbMaker")); // for St_tpcGasC
  assert( !gSystem->Load("StTpcDb"));
  assert( !gSystem->Load("StDbUtilities")); //for trigger simu
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StEEmcUtil"));
  assert( !gSystem->Load("StEEmcDbMaker"));
  assert( !gSystem->Load("StSpinDbMaker"));  
  assert( !gSystem->Load("StTriggerFilterMaker"));
  assert( !gSystem->Load("StTriggerUtilities"));

  // load jet libraries
  if (useJetFinder ==1 || useJetFinder == 2){ // jetfinder/jetreader libraries
    cout << "BEGIN: loading jetfinder libs" << endl;
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StRandomSelector");
    gSystem->Load("libfastjet.so");
    gSystem->Load("libsiscone.so");
    gSystem->Load("libsiscone_spherical.so");
    gSystem->Load("libfastjetplugins.so");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StJets");
    gSystem->Load("StJetEvent");
    gSystem->Load("StJetMaker");
    cout << "END: loading jetfinder libs" << endl;
  }
  else  {
    cout << "\nWARN: Jet are NOT read in, W-algo will not wrk properly\n " << endl;
  }

  // load W algo libraries
  assert( !gSystem->Load("StWalgo2011"));

  if(geant){ // libraries for access to MC record                                  
    assert( !gSystem->Load("StMcEvent"));      
    assert( !gSystem->Load("StMcEventMaker")); 
  }
  
  cout << " loading done " << endl;
  
  // create chain    
  chain = new StChain("StChain"); 
  // create histogram storage array  (everybody needs it):
  TObjArray* HList=new TObjArray;
  TObjArray* HListTpc=new TObjArray;

  int nFiles=1000;
  if(geant){ 
    //split MuDst file list to geant files
    ifstream infile(file); string line; 
    StFile *setFiles= new StFile(); int j=0;
    while(infile.good()){
      getline (infile,line);
      TString name = line;
      name.ReplaceAll(".MuDst.root",".geant.root");
      name.ReplaceAll("muDst/","geant/");
      if(line=="") break;
      setFiles->AddFile(name.Data());
      j++;
      if(j%10==0) cout<<"Added "<<j<<" files:"<<name.Data()<<endl;
      if(j==nFiles) break;
    }

    // get list of geant files                    
    StIOMaker* ioMaker = new StIOMaker("IO","r",setFiles,"bfcTree");
    //ioMaker->SetDebug();
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");   //deactivate all branches 
    ioMaker->SetBranch("geantBranch",0,"r");//activate geant Branch
  }

  // Now we add Makers to the chain...  
  muMk = new StMuDstMaker(0,0,muDir,file,"MuDst.root",nFiles);
#if 0
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
#endif
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
  }     
  else if (isMC<400) {
    if(isMC>=350) dbMk->SetMaxEntryTime(20130520,0); // run 12 embedding
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

  StSpinDbMaker *spDb=0;
  if(spinSort || useJetFinder == 1 )  spDb=new StSpinDbMaker("spinDb");


  if (useJetFinder == 1){// run jet finder
    double pi = atan(1.0)*4.0;    
    // Makers for clusterfinding
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
    
     // Jet maker
    StJetMaker2009* jetmaker = new StJetMaker2009;
    jetmaker->setJetFile(jetFile);

    // Set analysis cuts for 12-point branch
    StAnaPars* anapars12 = new StAnaPars;
    anapars12->useTpc  = true;
    anapars12->useBemc = true;
    anapars12->useEemc = true;
    anapars12->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));

    // TPC cuts
    anapars12->addTpcCut(new StjTrackCutFlag(0));
    anapars12->addTpcCut(new StjTrackCutNHits(12));
    anapars12->addTpcCut(new StjTrackCutPossibleHitRatio(0.51));
    anapars12->addTpcCut(new StjTrackCutDca(3));
    anapars12->addTpcCut(new StjTrackCutTdcaPtDependent);
    anapars12->addTpcCut(new StjTrackCutPt(0.2,200));
    anapars12->addTpcCut(new StjTrackCutEta(-2.5,2.5));
    anapars12->addTpcCut(new StjTrackCutLastPoint(125));

    // BEMC cuts
    anapars12->addBemcCut(new StjTowerEnergyCutBemcStatus(1));
    anapars12->addBemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
    anapars12->addBemcCut(new StjTowerEnergyCutEt(0.2));

    // EEMC cuts
    anapars12->addEemcCut(new StjTowerEnergyCutBemcStatus(1));
    anapars12->addEemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
    anapars12->addEemcCut(new StjTowerEnergyCutEt(0.2));
    
    // Jet cuts
    anapars12->addJetCut(new StProtoJetCutPt(3.5,200));
    anapars12->addJetCut(new StProtoJetCutEta(-100,100));

    // Set analysis cuts for 12-point noEEMC branch
    StAnaPars* anapars12_noEEMC = new StAnaPars;
    anapars12_noEEMC->useTpc  = true;
    anapars12_noEEMC->useBemc = true;
    anapars12_noEEMC->useEemc = true;
    anapars12_noEEMC->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));

    // TPC cuts
    anapars12_noEEMC->addTpcCut(new StjTrackCutFlag(0));
    anapars12_noEEMC->addTpcCut(new StjTrackCutNHits(12));
    anapars12_noEEMC->addTpcCut(new StjTrackCutPossibleHitRatio(0.51));
    anapars12_noEEMC->addTpcCut(new StjTrackCutDca(3));
    anapars12_noEEMC->addTpcCut(new StjTrackCutTdcaPtDependent);
    anapars12_noEEMC->addTpcCut(new StjTrackCutPt(0.2,200));
    anapars12_noEEMC->addTpcCut(new StjTrackCutEta(-2.5,2.5));
    anapars12_noEEMC->addTpcCut(new StjTrackCutLastPoint(125));

    // BEMC cuts
    anapars12_noEEMC->addBemcCut(new StjTowerEnergyCutBemcStatus(1));
    anapars12_noEEMC->addBemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
    anapars12_noEEMC->addBemcCut(new StjTowerEnergyCutEt(0.2));

    // EEMC cuts
    anapars12_noEEMC->addEemcCut(new StjTowerEnergyCutBemcStatus(1));
    anapars12_noEEMC->addEemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
    anapars12_noEEMC->addEemcCut(new StjTowerEnergyCutEt(10000.0)); // nothing is this high JS

    // Jet cuts
    anapars12_noEEMC->addJetCut(new StProtoJetCutPt(3.5,200));
    anapars12_noEEMC->addJetCut(new StProtoJetCutEta(-100,100));

    // Set anti-kt R=0.6 parameters
    StFastJetPars* AntiKtR060Pars = new StFastJetPars;
    AntiKtR060Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
    AntiKtR060Pars->setRparam(0.6);
    AntiKtR060Pars->setRecombinationScheme(StFastJetPars::E_scheme);
    AntiKtR060Pars->setStrategy(StFastJetPars::Best);
    AntiKtR060Pars->setPtMin(3.5);

    // Set anti-kt R=0.5 parameters
    StFastJetPars* AntiKtR050Pars = new StFastJetPars;
    AntiKtR050Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
    AntiKtR050Pars->setRparam(0.5);
    AntiKtR050Pars->setRecombinationScheme(StFastJetPars::E_scheme);
    AntiKtR050Pars->setStrategy(StFastJetPars::Best);
    AntiKtR050Pars->setPtMin(3.5);

    jetmaker->addBranch("AntiKtR060NHits12",anapars12,AntiKtR060Pars);
    jetmaker->addBranch("AntiKtR060NHits12_noEEMC",anapars12_noEEMC,AntiKtR060Pars);
    jetmaker->addBranch("AntiKtR050NHits12",anapars12,AntiKtR050Pars);
    jetmaker->addBranch("AntiKtR050NHits12_noEEMC",anapars12_noEEMC,AntiKtR050Pars);

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
      if(iev%100 == 0) cout<<"iev="<<iev<<endl;
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
  

  //.... W reconstruction code ....
  St2011WMaker *WmuMk=new St2011WMaker();
  if(isMC) { // MC specific
    WmuMk->setMC(isMC); //pass "version" of MC to maker
  }else {// real data
    WmuMk->setTrigID(idL2BWtrg,idL2EWtrg);
  }
  
  TString outFtree=wtreeDir; outFtree+=outF; outFtree+=".Wtree.root";
  WmuMk->setTreeName(outFtree);
  
  if (useJetFinder == 2) {
    cout << "Configure to read jet trees " << endl;
    WmuMk->chainJetFile(jetFile);
    WmuMk->setJetTreeBranch("AntiKtR060NHits12","AntiKtR060NHits12_noEEMC"); //select jet tree braches used
  }

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
  
  if(spinSort){
    WmuMk->attachSpinDb(spDb);
    enum {mxSM=5}; // to study eta-cuts, drop Q/PT cut
    St2011pubSpinMaker *spinMkA[mxSM];
    for(int kk=0;kk<mxSM;kk++) {
      char ttx[100]; sprintf(ttx,"%cspin",'A'+kk);
      printf("add spinMaker %s %d \n",ttx,kk);
      spinMkA[kk]=new St2011pubSpinMaker(ttx); 
      spinMkA[kk]->attachWalgoMaker(WmuMk);
      spinMkA[kk]->setHList(HList); 
      if(kk==1) spinMkA[kk]->setEta(-1.,0.);
      if(kk==2) spinMkA[kk]->setEta(0,1.);
      if(kk==3) spinMkA[kk]->setQPT(false);// disable Q/PT cut
      if(kk==4) spinMkA[kk]->setNoEEMC(); 
    }  
  }
 
  if(geant){
    pubMcMk=new St2011pubMcMaker("pubMc");
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
// Revision 1.14  2013/12/06 14:21:22  stevens4
// update jet library loading for SL6 and remove mid-point cone
//
// Revision 1.13  2013/10/11 14:22:34  stevens4
// changed order of library loading to be compatible with ROOT/SL upgradex
//
// Revision 1.12  2013/09/13 19:37:36  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.11  2013/07/03 16:53:15  stevens4
// Update for efficiency studies with embedding
//
// Revision 1.10  2012/09/18 22:33:07  stevens4
// remove hardcoded jet tree path
//
// Revision 1.8  2012/08/07 21:06:56  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.7  2012/07/13 16:11:49  balewski
// minor clenup, prevent crash in Finish if zero input events, now it runs on M-C events as well
//
// Revision 1.6  2012/07/12 20:49:26  balewski
// added spin info(star: bx48, bx7, spin4) and maxHtDSM & BTOW to Wtree
// removed dependence of spinSortingMaker from muDst
// Now Wtree can be spin-sorted w/o DB
// rdMu.C & readWtree.C macros modified
// tested so far on real data run 11
// lot of misc. code shuffling
//
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
