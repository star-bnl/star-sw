class StChain;
StChain *chain=0;
int useEtow=3;// 0=don't use; 1=only in event-display, 2=in away sum,3=in away&near sum

int spinSort=false;
int isJustin=false;
bool isRoss=true; 
int geant=false;

int rdMuWana(
	     int nEve=2e3,
	     char* inDir   = "",// make it empty for scheduler 
	     char* file    = "fillListD/F10434/R10085016_230530_230600.lis",
	     int nFiles  = 1000, // max # of muDst files
	     int isMC=1, // 0=run9-data, 1=Weve, 2=QCDeve, 3=Zeve, 20=rcf10010,... 26=rcf10016
	     int useJetFinder = 2, // 0 - no jets & crash; 1 generate jet trees; 2 read jet trees
             TString jetTreeDir = "/star/institutions/iucf/stevens4/wAnalysis/aps2010/jetTree/" //default location of jet trees to be used
 ) { 

  //jetTreeDir = "./";
  if(isMC==1) file  = "/star/institutions/mit/balewski/freezer/2009-W-algoVer4.3s-prelim-Jacobian2/fillListA/mcSetD1_ppWprod.lis";
  if(isMC==2) file  = "/star/institutions/mit/balewski/freezer/2009-W-algoVer4.3s-prelim-Jacobian2/fillListA/mcSetD2_ppQCD10_inf_filter.lis";
  if(isMC==3) file  = "/star/institutions/mit/balewski/freezer/2009-W-algoVer4.3s-prelim-Jacobian2/fillListA/mcSetD1_ppZprod.lis";
  if(isMC==4) file  = "/star/u/balewski/2009-Wana-pp500/fillListA/mcSetD1_ppWplusProd.lis";
  if(isMC==5) file  = "fillListA/mcSetD1_ppWminusProd.lis";
  if(isMC==6) file  = "fillListA/mcSetD1_ppWdec.lis";
  if(isMC==7) file  = "fillListA/mcSetD1_ppZdec.lis";
  if(isMC==8) geant=true; //uses geant files 

  //** official simu January 2010 , rcf10010-rcf10016
  //** because of hardcoding timestamp these MC muDst's  must be fed 
  //** individually by external file lists that can be found at 
  //** /star/institutions/iucf/stevens4/wAnalysis/aps2010/fillListD/
  //** and submitted to scheduler by scripts there 
  
  //single files for testing
  string dir = "/star/data56/reco/pp500/pythia6_422/";
  if(isMC==20) {
    file =Form("%sWplus_enu/perugia320/y2009a/gheisha_on/p09ig/rcf10010_1000_1000evts.MuDst.root",dir);
    geant=true;
  }
  if(isMC==21) file =Form("%sWminus_enu/perugia320/y2009a/gheisha_on/p09ig/rcf10011_1000_1000evts.MuDst.root",dir);
  if(isMC==22) file =Form("%sZ_eplus_eminus/perugia320/y2009a/gheisha_on/p09ig/rcf10014_1000_1000evts.MuDst.root",dir);
  if(isMC==23) file =Form("%sW_jet/perugia320/y2009a/gheisha_on/p09ig/rcf10013_1000_1000evts.MuDst.root",dir);
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
  else if(isMC==8){ //include geant.root files from private MC
    char *file1=strstr(file,"/pp")+1;                      
    printf("file1=%s=%s=\n",file1);                        
    outF=file1; outF.ReplaceAll(".MuDst.root","");          
    TString fileG=file; fileG.ReplaceAll("MuDst","geant"); 
    fileG.ReplaceAll("/mu/","/geant/");                     
  }
  else if(isMC>=20){ //official rcf MC 
    char *file1=strstr(file,"/rcf")+1;
    printf("file1=%s=%s=\n",file1);
    outF=file1; outF.ReplaceAll(".MuDst.root","");
    TString fileG=file; fileG.ReplaceAll("MuDst","geant");
  }
  else { // private MC events
    char *file1=strstr(file,"/mc")+1;
    printf("file1=%s=%s=\n",file1);
    outF=file1; file1=outF.Data();
    outF.ReplaceAll(".lis",""); 
  }
  printf("TRIG ID: bht3=%d l2w=%d isMC=%d\n",bht3ID,l2wID,isMC);
  
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gROOT->LoadMacro("LoadLogger.C");
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");
  
  assert( !gSystem->Load("StDaqLib"));

  // libraries below are needed for DB interface
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StDetectorDbMaker")); // for St_tpcGasC
  assert( !gSystem->Load("StEEmcUtil"));
  assert( !gSystem->Load("StEEmcDbMaker"));
  cout << " loading done " << endl;
  assert( !gSystem->Load("StWalgoB2009"));
  if(spinSort)  assert( !gSystem->Load("StSpinDbMaker"));
  
  if (useJetFinder ==1 || useJetFinder == 2){ // jetfinder/jetreader libraries
    cout << "BEGIN: loading jetfinder libs" << endl;
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StJets");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StJetEvent");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");
    cout << "END: loading jetfinder libs" << endl;
  }
  else  {
    cout << "Jets finder is not configured: exiting" << endl;
    return;
  }

  // libraries for access to MC record         
  if(geant){                                  
    assert( !gSystem->Load("StMcEvent"));      
    assert( !gSystem->Load("StMcEventMaker")); 
  }
  
  // create chain    
  chain = new StChain("StChain"); 
  
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


  if(geant){                                            
    StMcEventMaker *mcEventMaker = new StMcEventMaker();  
    mcEventMaker->doPrintEventInfo = false;               
    mcEventMaker->doPrintMemoryInfo = false;              
  }

  St_db_Maker   *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
  if(isMC>0 && isMC<20) { // private 2008 M-C samples
    // .... Barrel....
    dbMk->SetFlavor("sim","bemcPed"); // set all ped=0
    dbMk->SetFlavor("sim","bemcStatus");  // ideal, all=on
    dbMk->SetFlavor("sim","bemcCalib"); // use ideal gains 
    dbMk->SetFlavor("sim","bsmdePed"); // set all ped=0
    dbMk->SetFlavor("sim","bsmdeCalib"); // load those, not used in algo
    dbMk->SetFlavor("sim","bsmdeStatus"); // ideal, all=on
    
    dbMk->SetFlavor("sim","bsmdpPed");   // set all ped=0
    dbMk->SetFlavor("sim","bsmdpCalib"); // load those, not used in algo
    dbMk->SetFlavor("sim","bsmdpStatus"); // ideal, all=on

    //.... Endcap.....
    dbMk->SetFlavor("sim","eemcPMTcal");
    dbMk->SetFlavor("sim","eemcPMTstat");
    dbMk->SetFlavor("sim","eemcPMTped");
  } 
  else if (isMC==0) { // run 9 data
    dbMk->SetFlavor("Wbose","bsmdeCalib"); // Willie's relative gains E-plane
    dbMk->SetFlavor("Wbose","bsmdpCalib"); // P-plane
    dbMk->SetFlavor("missetTCD","eemcPMTcal");  // ETOW gains , not-standard
    dbMk->SetFlavor("sim","bemcCalib"); // use ideal gains for 2009 real data as well
  }  
  if(isMC>=20) { // official rcf1001N M-C samples, January 2010
    dbMk->SetMaxEntryTime(20100124,0); //use 'ofl' beginTime 2008-12-15 00:00:02 BTOW 'ofl' gains these MC were generated with
    dbMk->SetFlavor("sim","eemcPMTped"); // to compensate action of fast simu
    dbMk->SetFlavor("sim","eemcPMTcal"); // to compensate action of fast simu

    //get timestamp from static txt file
    int timestamp=-999;
    ifstream mcTimeStamp("/star/institutions/iucf/stevens4/wAnalysis/aps2010/mcRcfSDT.txt");
    string line;
    while(mcTimeStamp.good()){ //loop over file names
      getline(mcTimeStamp,line);
      if(mcTimeStamp.eof()) break;
      string fileName = line.substr(0,33);
      if(fileName.compare(file1)==0){ //find correct timestamp
	timestamp = atoi(line.substr(34,8).data());
	break;
      }
    }
    if(timestamp==-999) {
      cout<<"Unknown input file, no timestamp found"<<endl;
      return 0; 
    }
    cout<<fileName<<" "<<timestamp<<endl;
    dbMk->SetDateTime(timestamp,0); //set timestamp
  }
    
  //.... load EEMC database
  StEEmcDbMaker*  mEEmcDatabase = new StEEmcDbMaker("eemcDb");
  
  //.... Jet finder code ....
  if (useJetFinder > 0)  {
    cout << "BEGIN: running jet finder " << endl;
    TString outFile = "jets_"+outF+".root";
  }
  if (useJetFinder == 1){
    double pi = atan(1.0)*4.0;
    
    // Makers for clusterfinding
    StSpinDbMaker* spDbMaker = new StSpinDbMaker("spinDb");
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
    StPreEclMaker *pre_ecl=new StPreEclMaker();
    StEpcMaker *epc=new StEpcMaker();

    //test Mike's new 4p maker:
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
    anapars->setNhits(5); //track->nHitsFit()>10
    anapars->setCutPtMin(0.2); //track->pt() > 0.2
    anapars->setAbsEtaMax(2.0); //abs(track->eta())<1.6
    anapars->setJetPtMin(3.5);
    anapars->setJetEtaMax(100.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);

    //Setup the cone finder (See StJetFinder/StConeJetFinder.h -> class StConePars)
    StConePars* cpars = new StConePars();
    cpars->setGridSpacing(105, -3.0, 3.0, 120, -pi, pi);  //include EEMC
    cpars->setConeRadius(0.7);
    cpars->setSeedEtMin(0.5);
    cpars->setAssocEtMin(0.1);
    cpars->setSplitFraction(0.5);
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);
    cpars->setDebug(false);
    
    emcJetMaker->addAnalyzer(anapars, cpars, bet4pMakerFrac100, "ConeJets5_100"); //100% subtraction 
    emcJetMaker->addAnalyzer(anapars, cpars, bet4pMakerFrac100_noEEMC, "ConeJets5_100_noEEMC"); //100% subtraction (no Endcap)

    anapars->setNhits(12); //track->nHitsFit()>12
    emcJetMaker->addAnalyzer(anapars, cpars, bet4pMakerFrac100, "ConeJets12_100"); //100% subtraction 
    emcJetMaker->addAnalyzer(anapars, cpars, bet4pMakerFrac100_noEEMC, "ConeJets12_100_noEEMC"); //100% subtraction (no Endcap)

    //Tight cuts (esp. SMD)
    pre_ecl->SetClusterConditions("bemc", 4, 0.4, 0.05, 0.02, kFALSE);
    pre_ecl->SetClusterConditions("bsmde", 5, 0.4,0.005, 0.1,kFALSE);
    pre_ecl->SetClusterConditions("bsmdp", 5, 0.4,0.005, 0.1,kFALSE);
    pre_ecl->SetClusterConditions("bprs", 1, 500., 500., 501., kFALSE);

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
      if(stat) break; // EOF or input error
        eventCounter++;
    }
    cout<<"R"<<runNo<<" nEve="<<eventCounter<<" total "; tt.Print();
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
    StJetReader *jetReader = new StJetReader("JetReader",muMk);
    jetReader->InitFile(jetTreeDir+outFile);
  }


  //.... W reconstruction code ....
  St2009WMaker *WmuMk=new St2009WMaker();
  if(isMC) { // MC specific
    WmuMk->setMC(isMC); //pass "version" of MC to maker
    //vary energy scales for syst. uncert. calc.
    //WmuMk->setJetNeutScaleMC(1.0);  //vary by 7.5%
    //WmuMk->setJetChrgScaleMC(1.0); //vary by 5.6%
    //WmuMk->setBtowScale(1.0);     //vary by 7.5%
    //WmuMk->setEtowScale(1.0);     //vary by 10%
    if(isMC<20) WmuMk->setEtowScale(1.3); // rcf simu should have correct SF
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
  
  WpubMk=new St2009pubWanaMaker("pubJan"); 
  WpubMk->attachWalgoMaker(WmuMk); // 

  //Collect all output histograms   
  TObjArray* HList=new TObjArray;
  WmuMk->setHList(HList);
  WpubMk->setHList(HList);
  
  if(spinSort){
    spDb=new StSpinDbMaker("spinDb");
    enum {mxSM=4}; // to study eta-cuts, drop Q/PT cut
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
    }  
  }
  
  if(geant){
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
    if(stat) break; // EOF or input error
    eventCounter++;    
  }

  cout<<"R"<<runNo<<" nEve="<<eventCounter<<" total "; tt.Print();
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
// Revision 1.25  2010/03/20 19:19:07  balewski
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
