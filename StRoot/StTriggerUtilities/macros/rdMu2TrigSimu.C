// Before running macro create an output directory "outL2" if you want a copy of the L2 output histos
// This macro runs on real data (flagMC=0) and MC files (flagMC=1)
// Set flag ==1 for those detectors you want included in the trigger decisions 
// Set configuration for BEMC (online, offline or custom).  EEMC is the same for offline and online
// Choose the correct configuration year if you want to use L2

int total=0;
class StChain *chain=0; 
char *eemcSetupPath="/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/";

void rdMu2TrigSimu(char *file="/star/data47/reco/pp200/pythia6_410/9_11gev/cdf_a/y2006c/gheisha_on/p07ic/rcf1309_*_2000evts.MuDst.root"){
  
  int nevents = 200;
  int flagMC=1;  // 0/1 == Using Real/Simulation data files 
  int useEemc=1; // 0/1 == Exclude/Include EEMC in Trigger Decisions 
  int useBemc=1; // 0/1 == Exclude/Include BEMC in Trigger Decisions 
  int useL2=1;   // 0/1 == Exclude/Include L2 in Trigger Decisions 
  int L2ConfigYear=2006; // possible: 2006, 2008
  int bemcConfig=2; // Online==1, Offline==2, Expert==3
  int playConfig=0; // jan:100_199
  int emcEveDump=0; // extrating raw EMC data in a custom format
  int outputL2Histo=0;//output L2 histos to directory outL2
  TString outDir="./outL2/"; 


  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  assert( !gSystem->Load("StDetectorDbMaker"));
  assert( !gSystem->Load("StDbUtilities"));
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StEEmcUtil")); // needed by eemcDb
  assert( !gSystem->Load("StEEmcDbMaker"));
  assert( !gSystem->Load("StDaqLib")); // needed by bemcDb
  assert( !gSystem->Load("StEmcRawMaker"));
  assert( !gSystem->Load("StEmcADCtoEMaker"));
  if (flagMC) {
    assert( !gSystem->Load("StMcEvent"));
    assert( !gSystem->Load("StMcEventMaker"));
    assert( !gSystem->Load("StEmcSimulatorMaker"));
    assert( !gSystem->Load("StEEmcSimulatorMaker"));
    assert( !gSystem->Load("StEpcMaker"));
  }
  assert( !gSystem->Load("StTriggerUtilities"));

  gROOT->Macro("LoadLogger.C");
  cout << " loading done " << endl;
  
  chain= new StChain("StChain"); 

  if (flagMC){
    TString geantFile;
    geantFile += file;
    geantFile.ReplaceAll("MuDst.root", "geant.root");
    printf("geantFile=%s\n", geantFile.Data());
    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(geantFile);
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");             //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
    StMcEventMaker *evtMaker = new StMcEventMaker();
  }

  //Need MuDstMaker to get data
  printf(" Analyzing file=%s\n",file);  
  StMuDstMaker* muDstMaker =new StMuDstMaker(0,0,"",file,"",1000);

  //Database -- get a real calibration from the database
  St_db_Maker* dbMk =0;
  if(useEemc || useL2) // full DB access
    dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");
  else // only Barrel is uploaded, is faster 
    dbMk  = new St_db_Maker("Calibrations","MySQL:Calibrations_emc");

    
  //If MC then must set database time and date
  //If Endcap fast simu is used tower gains in DB do not matter,JB
  if (flagMC) dbMk->SetDateTime(20060522, 55000);//timestamp R7142018
  
  //Collect all output histograms 
  TObjArray* HList=new TObjArray; 
  
  //Endcap DB
  if(useEemc || useL2) new StEEmcDbMaker("eemcDb");
  

  //Get BEMC adc values
  if (flagMC && useBemc) {
    StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
    if (bemcConfig == 1) {
        emcSim->setCheckStatus(kBarrelEmcTowerId,false); //this returns hits regardless of offline tower status
    }
    emcSim->setCalibSpread(kBarrelEmcTowerId,0.15);//spread gains by 15%
  }
  if (flagMC==0 && useBemc){
    StEmcADCtoEMaker *bemcAdc = new StEmcADCtoEMaker();//for real data this sets calibration and status
    if (bemcConfig == 1) {
        bemcAdc->setCheckStatus(kBarrelEmcTowerId,false);
    }
  }

  //must use slow simulator to get pedestals correct for L2
  if (flagMC==1 && useEemc){
    StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
    slowSim->setSamplingFraction(0.0384); // effectively scales all Tower energies with a factor of 1.3 (added by: Ilya Selyuzhenkov; April 11, 2008)
    slowSim->setAddPed(true);
    slowSim->setSmearPed(true);
  }
 
  //Get TriggerMaker
  StTriggerSimuMaker *simuTrig = new StTriggerSimuMaker("StarTrigSimu");
  simuTrig->setHList(HList);
  simuTrig->setMC(flagMC); // must be before individual detectors, to be passed
  simuTrig->useBbc();
  if(useEemc) {
    simuTrig->useEemc(0);//default=0:just process ADC, 1,2:comp w/trgData,see .
    simuTrig->eemc->setSetupPath(eemcSetupPath);
  } 
  if(useBemc){
    simuTrig->useBemc();
    simuTrig->bemc->setConfig(bemcConfig);
  }

  if(flagMC && useEemc==2){
    // pass one argument to M-C as generic switch    
    // Endcap specific params -- ok Jan you need to change this to a default "online" setup
    int eemcDsmSetup[20]; // see StEemcTriggerSimu::initRun() for definition
    memset(eemcDsmSetup, 0,sizeof(eemcDsmSetup));// clear all, may be a bad default
    eemcDsmSetup[0]=3;  // HTthr0
    eemcDsmSetup[1]=12; // HTthr1
    eemcDsmSetup[2]=22; // HTthr2
    eemcDsmSetup[3]=1;  // TPthr0
    eemcDsmSetup[4]=17; // TPthr1
    eemcDsmSetup[5]=31; // TPthr2
    eemcDsmSetup[10]=2; //HTTPthrSelc, 2=use_thres_#1
    simuTrig->eemc->setDsmSetup(eemcDsmSetup);    
  }


  if(useL2) {
    /* 
       reads all input/setup files from  L2setup-yyyymmdd/
       writes all output files to L2out-yyyymmdd 
       depending on the DB time stamp 
       both dierectiorie MUST exist, setup must be reasonable
    */
    StGenericL2Emulator* simL2Mk=0;
    if(L2ConfigYear==2006) simL2Mk= new StL2_2006EmulatorMaker;
    else if(L2ConfigYear==2008) simL2Mk= new StL2_2008EmulatorMaker;
    assert(simL2Mk);
    simL2Mk->setSetupPath(eemcSetupPath);
    simL2Mk->setOutPath(outDir.Data());
    if (flagMC) simL2Mk->setMC();
    simuTrig->useL2(simL2Mk);
  }

  
  //if(emcEveDump) new StJanEventMaker;
    
  StTriggerSimuPlayMaker *playMk= new StTriggerSimuPlayMaker; // to develope user  analysis of trigQA 
  playMk->setConfig(playConfig);
  playMk->setHList(HList);
  
  
  chain->ls(3);
  chain->Init();
 
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "\n****************************************** " << endl;
    cout << "Working on eventNumber:\t" << iev <<"\tof:\t"<<nevents<<endl;
    cout << "****************************************** " << endl;
    chain->Clear();
    int iret = chain->Make(iev);
    total++;   
    if (iret % 10 == kStEOF || iret % 10 == kStFatal)  {
      cout << "Bad return code!" << endl;
      break;
    }

    int trigID[3]={127213,117211,137611};
    StMuDst *muDst = muDstMaker->muDst();
    StMuEvent *muEvent = muDst->event();    
    StMuTriggerIdCollection trig = muEvent -> triggerIdCollection();
    StTriggerId l1trig = trig.nominal();
    if( l1trig.isTrigger(trigID[0])) {
      cout<<" SimuTrigger 127213 ="<<simuTrig->isTrigger(trigID[0])<<" BEMC="<<simuTrig->bemc->triggerDecision(trigID[0])<<" L2="<<simuTrig->lTwo->triggerDecision(trigID[0])<<endl;
     }
    if( l1trig.isTrigger(trigID[1])) {
      cout<<" SimuTrigger 117211 ="<<simuTrig->isTrigger(trigID[1])<<" BEMC="<<simuTrig->bemc->triggerDecision(trigID[1])<<" L2="<<simuTrig->lTwo->triggerDecision(trigID[1])<<endl;
     }
    if( l1trig.isTrigger(trigID[2])) {
      cout<<" SimuTrigger 137611 ="<<simuTrig->isTrigger(trigID[2])<<" BEMC="<<simuTrig->bemc->triggerDecision(trigID[2])<<" L2="<<simuTrig->lTwo->triggerDecision(trigID[2])<<endl;
    }

    
    StTriggerSimuResult trigResult = simuTrig->detailedResult(trigID[2]);
    if (trigResult.bemcDecision()==1){
       vector<short> towerId = trigResult.highTowerIds();
      for (unsigned i=0; i<towerId.size(); i++) {
      	cout<<" LO Trigger BEMC Tower="<<towerId[i]<<" adc="<<trigResult.highTowerAdc(towerId[i])<<endl;
      }
    }


   if (trigResult.l2Decision()==1){
       vector<short> towerId = trigResult.highTowerIds();
      for (unsigned i=0; i<towerId.size(); i++) {
      	cout<<" L2 Trigger BEMC Tower="<<towerId[i]<<" adc="<<trigResult.highTowerAdc(towerId[i])<<endl;
      }
    }
 
  }

 
  chain->Finish();
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;


  if (outputL2Histo==1) {
  
    TString fileMu=file;
    printf("=%s=\n",fileMu.Data());
    if(fileMu.Contains(".lis")) fileMu.ReplaceAll(".lis",".trgSim");
    if(fileMu.Contains(".MuDst.root")) fileMu.ReplaceAll(".MuDst.root",".trgSim");
    TString outF=outDir+fileMu;
    outF+=".hist.root";
    printf("=%s=\n",outF.Data());
    hf=new TFile(outF,"recreate");
    if(hf->IsOpen()) {
      HList->Write();
      printf("\n Histo saved -->%s<\n",outF.Data());
    } 
    else {
      printf("\n Failed to open Histo-file -->%s<, continue\n",outF.Data());
    }
  }
  
  //cout <<Form("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n\n",total,nEntries,rate,tMnt)<<endl;
 
}




//========================================
//========================================
#if 0
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
clean it up, Jan

  const int mxTr=13;
  int myTrgList[mxTr]    ={127580,127551,127271,127571,127821,127831,127575,127622,127221,127611,117705,127501,127652};
  // prescale as for run 7101015
  char* myTrgListN[mxTr]={"eemc-http*116","eemc-jp0-mb*221", "eemc-jp1-mb*1", "bemc-jp1*433", 
			  "bemc-http-mb-fast*1", "eemc-http-mb-fast*1", "bemc-jp0-etot*979" , 
			  "bemc-jp0-etot-mb-L2jet*2", "bemc-jp1-mb*1", "bemc-http-mb-l2gamma*1",
			  "jpsi-mb*20", "bemc-jp0-mb*909","eemc-jp0-etot-mb-L2jet*2"};
  int nR[mxTr],nS[mxTr],nRS[mxTr];
  memset(nR,0, sizeof(nR));
  memset(nS,0, sizeof(nR));
  memset(nRS,0, sizeof(nR));

  int BL1_ADC[6];
    cout<<Form(" Simu trgSize=%d, firedID:", simuTrig->mTriggerList.size())<<endl;
    int j;
    for(j=0; j<simuTrig->mTriggerList.size();j++) 
      cout<<Form("s%d, ", simuTrig->mTriggerList[j]);
    cout<<"\n";
      

     // Access to muDst .......................
    StMuEvent* muEve = muDstMaker->muDst()->event();

    StEventInfo &info=muEve->eventInfo();
    StMuTriggerIdCollection &tic=muEve->triggerIdCollection();
    vector<unsigned int> trgL=tic.nominal().triggerIds();
    cout<<Form(" real trgSize=%d, firedID:",trgL.size());
    for(j=0; j<trgL.size();j++) 
      cout<<Form("r%d, ", trgL[j]);
    cout<<"\n";

    //.. compare Simu vs.  real
    for(j=0;j<mxTr;j++) {
      int trgId=myTrgList[j];
      bool realT=tic.nominal().isTrigger(trgId);
      bool simT=simuTrig->isTrigger(trgId); // endcap only, tmp
      if(realT) nR[j]++;
      if(realT && simT) nRS[j]++;
      if(simT) nS[j]++;
        cout <<Form("C:j=%d  trg=%d  R=%d S=%d  RS=%d",j, myTrgList[j],realT,simT,realT && simT )<<endl;
     }

   for(j=0;j<mxTr;j++) {
     cout <<Form("SUM  trg=%d  nR=%d nS=%d  nRS=%d  %s", myTrgList[j], nR[j],nS[j],nRS[j],myTrgListN[j])<<endl;
   }
 

#endif
