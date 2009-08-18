#include "TString.h"
#include "assert.h"
void doPhoton(unsigned long nEvents, char *infile,char *jobid,char *flag,char *coll,Int_t nFiles=0,char *timestampfile) 
{
  Bool_t debug=kFALSE;//macro debug
  Bool_t debugg=kFALSE;

  Bool_t USERANDOMIZER=kFALSE;

  gSystem->Load("StarRoot");
  gSystem->Load("St_base");

  //for logger
  gSystem->Load("liblog4cxx.so");
  gSystem->Load("StStarLogger.so");
  StLoggerManager::StarLoggerInit();
  //--

  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StMuDSTMaker");

  gSystem->Load("StBichsel");

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  // libraries for the DB
  gSystem->Load("StDaqLib");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
   
  //gSystem->Load("StEmcDecoder");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");

  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StEmcMixerMaker");

  gSystem->Load("StEmcTriggerMaker");

  //load timerandomizer:
  gSystem->Load("StTimeRandomizerMaker");

  gSystem->Load("/star/u/russcher/MyEvent/MyEvent");
  gSystem->Load("StPhotonMaker");

  cout<<"Finished loading libraries"<<endl;

  StChain* chain = new StChain("bfc"); 
  
  StEmcADCtoEMaker* adcMaker=0;
  StPreEclMaker* preEcl=0;

  StTimeRandomizerMaker* rndMaker=0;
  St_db_Maker* dbMk=0;
  
  StDetectorDbMaker* detDbMk=0;

  TString output(jobid);
  output.Append("_");
  output.Append(coll);
  output.Append("_");
  output.Append(flag);
  output.Append(".root");

  TString input(infile);
  if(strcmp(flag,"mc")==0||strcmp(flag,"realembed")==0||strcmp(flag,"embed")==0){
    if(input.EndsWith("list")){
      cout<<endl<<"running on event.root, prepend @ on "<<input.Data()<<endl;
      input.Prepend("@");
    }
  }
  
  StIOMaker* ioMaker=0;
  StMcEventMaker* mcEventMaker=0;
  StEmcSimulatorMaker* emcSim=0;
 
  //real data chain, for mudst
  if(strcmp(flag,"real")==0){
    cout << "Processing REAL data" << endl;
    
    if(nFiles==0){
      cout<<"running on MuDst without number of files specified!!!"<<endl;
      return;
    }
    StMuDstMaker* mudstMaker = new StMuDstMaker(0,0,"",input.Data(),"",nFiles);
    dbMk = new St_db_Maker("db","MySQL:StarDb", "$STAR/StarDb");
    detDbMk = new StDetectorDbMaker();     
    StMuDbReader* dbReader = StMuDbReader::instance();
    StMuDst2StEventMaker *m2e = new StMuDst2StEventMaker();
  }
  
  //real data chain, for event.root
  if(strcmp(flag,"realembed")==0){
    cout << "Processing REALEMBED data" << endl;
    
    ioMaker = new StIOMaker("IO","r",input.Data());
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");
    ioMaker->SetBranch("eventBranch", 0, "r");
    
    dbMk = new St_db_Maker("db","MySQL:StarDb", "$STAR/StarDb");
  }
 
  //plain monte carlo chain
  if(strcmp(flag,"mc")==0)
    {
      ioMaker = new StIOMaker("IO","r",input.Data());
      ioMaker->SetIOMode("r");
      ioMaker->SetBranch("*",0,"0");
      ioMaker->SetBranch("eventBranch", 0, "r");
      ioMaker->SetBranch("geantBranch",0,"r");

      //Oleksandr's timerandomizer:
      if(USERANDOMIZER){
	rndMaker=new StTimeRandomizerMaker();
	rndMaker->setRunTimesFilename(timestampfile);
	rndMaker->setNormalizeEventsTotal(3000);
	rndMaker->setDatasetNameStEvent("IO_Root/.data/bfcTree/eventBranch/StEvent");
	rndMaker->setSeed(0);
	rndMaker->setBaseEventId(0);
      }
      
      dbMk = new St_db_Maker("db","MySQL:StarDb", "$STAR/StarDb");
      if(strcmp(coll,"dAu")==0 && !USERANDOMIZER){
	dbMk->SetDateTime(20030313,000000);
      }
      if(strcmp(coll,"pp05")==0 && !USERANDOMIZER){
	int date=20050508;
	int time=000001;
     	dbMk->SetDateTime(date,time);
      }
      mcEventMaker = new StMcEventMaker();
      mcEventMaker->doPrintEventInfo=debug;
      mcEventMaker->doPrintMemoryInfo=debug;
      mcEventMaker->doPrintCpuInfo=debug;

      cout << "Processing MC data" << endl;
      
      // for MC data StEmcSimulatorMaker should be used instead of StEmcADCtoEMaker
      emcSim = new StEmcSimulatorMaker();
      emcSim->setCalibOffset(kBarrelEmcTowerId,0.0);//bemc
      emcSim->setCalibOffset(kBarrelSmdEtaStripId,0.0);//bsmde
      emcSim->setCalibOffset(kBarrelSmdPhiStripId,0.0);//bsmdp
      emcSim->setCalibSpread(kBarrelSmdEtaStripId,0.2);
      emcSim->setCalibSpread(kBarrelSmdPhiStripId,0.2);
      if(strcmp(coll,"dAu")==0){
	emcSim->setCalibSpread(kBarrelEmcTowerId,0.1);
      }
      if(strcmp(coll,"pp05")==0){
	emcSim->setCalibSpread(kBarrelEmcTowerId,0.07);
      }
      //set bsmd ADC saturation:
      //emcSim->setMaximumAdc(kBarrelSmdEtaStripId,650.);
      //emcSim->setMaximumAdc(kBarrelSmdPhiStripId,650.);
    }
  //embedding chain
  if(strcmp(flag,"embed")==0){
    ioMaker = new StIOMaker("IO","r",input.Data());
    ioMaker->SetDebug(debug);
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");
    ioMaker->SetBranch("eventBranch", 0, "r");
    ioMaker->SetBranch("geantBranch",0,"r");
    
    cout << "Processing EMBED data" << endl;
    
    dbMk=new St_db_Maker("db","MySQL:StarDb", "$STAR/StarDb");
    cout<<"database done"<<endl;
    
    adcMaker=new StEmcADCtoEMaker();
    adcMaker->setPrint(debug);
    adcMaker->saveAllStEvent(kTRUE);
    cout<<"first adcmaker done"<<endl;
    
    StEmcPreMixerMaker* preMixer=new StEmcPreMixerMaker("preEmbed");
    cout<<"added premixer"<<endl;
    
    mcEventMaker=new StMcEventMaker();
    mcEventMaker->doPrintEventInfo=debug;
    mcEventMaker->doPrintMemoryInfo=debug;
    mcEventMaker->doPrintCpuInfo=debug;
    cout<<"added mcEventmaker"<<endl;
    
    emcSim = new StEmcSimulatorMaker();
    cout<<"adjust for new simulator!"<<endl;
    return;    

    StEmcMixerMaker* emcMix=new StEmcMixerMaker();
    emcMix->setEmbedAll(kTRUE);//save all embed. hits, if no real hits too
    emcMix->setPrint(debug);
    cout<<"mixer done"<<endl;
    
  }
  
  StEmcADCtoEMaker *adc=0;
  //in case of embed. rerun, for "real data" first run.
  if(strcmp(flag,"mc")!=0){
    adc=new StEmcADCtoEMaker("EReadEmbed2");
    adc->setPrint(debug);
    if(strcmp(flag,"embed")==0) adc->setEmbeddingMode(kTRUE);
  }
  
  preEcl = new StPreEclMaker();
  StEpcMaker *epc = new StEpcMaker();
  //preEcl->setAlgorithm(kEmcClOld);
  preEcl->setPrint(debug);
  epc->setPrint(debug);

  StEmcTriggerMaker *emcTrigger=new StEmcTriggerMaker();
  emcTrigger->setDbMaker(dbMk);

  StPhotonMaker* photonMaker = new StPhotonMaker("photonMaker",output.Data(),flag,coll,debugg);
  photonMaker->setDbMaker(dbMk); 
  if(strcmp(flag,"mc")!=0) photonMaker->setAdcMaker(adc);
  
  chain->Init(); 
  
  if(preEcl){
    cout<<endl<<"Using old clustering, setting parameters now!"<<endl;
    preEcl->SetClusterConditions("bemc",  4, .35, .0350, 0.02, kFALSE);
    preEcl->SetClusterConditions("bprs",  1, 500., 500., 501., kFALSE);
    preEcl->SetClusterConditions("bsmde", 5, .20, .0005, 0.10, kFALSE);
    preEcl->SetClusterConditions("bsmdp", 5, .20, .0005, 0.10, kFALSE);
  }
  
  cout<<endl<<"******** Starting to loop over chain->Make()!!!! *********"<<endl<<endl;
  
  for(unsigned long iev=0;iev<nEvents; iev++){
    if(iev%100==0) cout<<"processing event: "<<iev<<endl;
    chain->Clear();
    Int_t iret = chain->Make(iev); 
    if(iev%100==0){
      //gObjectTable.Print();
      photonMaker->saveHistograms();         
    }
    if(iret==2) cout<<"### Last  Event Processed. Status = "<<iret<<endl; 
    if(iret==3) cout<<"### Error Event Processed. Status = "<<iret<<endl; 
    if(iret==4) cout<<"### Fatal Event Processed. Status = "<<iret<<endl;
    
    if(iret>1) break;
  } 
  

  chain->Finish(); 
  
}








