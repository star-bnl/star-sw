  
////////////////////////////////////////////////////////////////////
// Macro to run BEMC data analysis from StEvent
//
// AAPSuaide SEP-2004
//
////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

void doBemcMuDst(bool StEv = true, char* list = "test.list", int nFiles = 5, int nevents = 2000000)
{
  Bool_t  debug            = kFALSE; // debug mode. If true, prints a lot of information
  Bool_t  fillExtraHisto   = kFALSE; // create and fill extra QA histograms 
  Bool_t  saveAllStEvent   = kFALSE; // flag to save all hits into StEvent. If false, use ADCtoEMaker cuts 

  cout <<"list = "<<list<<endl;
  cout <<"nFiles = "<<nFiles<<endl;
  cout <<"nevents = "<<nevents<<endl;
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");

  // Do not forget to load your personal libraries  

  /////////////////////////////////////////////////////////////
  // create chain    
  /////////////////////////////////////////////////////////////
  chain = new StChain("StChain"); 
  if(debug) chain->SetDebug();
   
  /////////////////////////////////////////////////////////////
  // Now we add Makers to the chain...     
  /////////////////////////////////////////////////////////////
  StMuDstMaker* muDst = new StMuDstMaker(0,0,"",list,"",nFiles);
  StMuDbReader* muDB = StMuDbReader::instance();
  if(StEv) StMuDst2StEventMaker *muDst2StEvent = new StMuDst2StEventMaker();
  
  /////////////////////////////////////////////////////////////
  // This is the DB Maker and it is the interface to Star database 
  St_db_Maker *db = new St_db_Maker("StarDb","MySQL:StarDb");
  // if you want to use a different flavor for some table in the database
  // use the SetFlavor method in the St_db_Maker.
  // the defaul flavor for all the tables if "ofl"
  //
  // This example set the bemc status table to the table used by the pi0 analysis
  // db->SetFlavor("pi0","bemcStatus");

  /////////////////////////////////////////////////////////////
  // BEMC Reco makers start here...     
  /////////////////////////////////////////////////////////////
  
  // This is the maker that applies the calibration to the data
  StEmcADCtoEMaker *adc=new StEmcADCtoEMaker();
  adc->saveAllStEvent(saveAllStEvent); // if set to kTRUE, saves all the hits into StEvent.
  adc->setPrint(debug); // if true, prints extra information 
  controlADCtoE_st* c = adc->getControlTable(); // get control table. You can have access to detailed configuration for each sub detector
   
  /////////////////////////////////////////////////////////////
  // the constol table has the following parameters
  // short DeductPedestal[det-1];    // switch for deducting pedestal, should be = 1 to deduct pedestals 
  // short Calibration[det-1];       // switch for calibration, should be = 1 to do the calibration    
  // float CutOff[det-1];            // cutoff value for hits close to pedestal. -1 = none 
  // short CutOffType[det-1];        // cutoff type (0 = energy, 1 = pedestal RMS) 
  // short OnlyCalibrated[det-1];    // save only calibrated hits 
  // short messLimit;            // limit for warning message 
  // 
  // where 1 = bemc; 2 = bprs; 3 = bsmde; 4 = bsmdp;
  /////////////////////////////////////////////////////////////  
   
  // This maker does clustering in the detectors
  StPreEclMaker *ecl=new StPreEclMaker();
  ecl->setFillHisto(fillExtraHisto); // if kTRUE it fillls extra histograms
  ecl->setPrint(debug); // if true, prints extra information
  
  // This maker finds the BEMC points (matching)
  StEpcMaker *epc = new StEpcMaker();
  epc->setFillHisto(fillExtraHisto); // if kTRUE it fillls extra histograms
  epc->setPrint(debug); // if true, prints extra information
  
  /////////////////////////////////////////////////////////////
  // you instantiate your analysis maker here
  /////////////////////////////////////////////////////////////
  
  chain->Init();  
  
  /////////////////////////////////////////////////////////////
  // the EMC cluster finder parameters should be set after Init() method
  // is called
  // the syntax for SetClusterConditions is
  // SetClusterConditions("det Name", sizeMax, energySeed, energyAdd, energyThresholdAll, kCheckClustersOk)
  //   sizeMax is the maximum size the cluster can have 
  //   energySeed is the energy threshold to start looking for a cluster in that detector region
  //   energyAdd is the minimum energy a hit can have to be included as part of a cluster
  //   energyThresholdAll is the minimum energy a cluster can have in order to be saved
  //   kCheckClustersOk is a flag to do a refit of the cluster based on the shower profile. NOT TESTED !!!
  //
  // These are the default conditions
  /////////////////////////////////////////////////////////////
  ecl->SetClusterConditions("bemc",  4, 0.7, 0.001, 0.1, kFALSE);
  ecl->SetClusterConditions("bprs",  1, 0.1, 0.001, 0.1, kFALSE); 
  ecl->SetClusterConditions("bsmde", 5, 0.4, 0.001, 0.1, kFALSE);
  ecl->SetClusterConditions("bsmdp", 5, 0.4, 0.001, 0.1, kFALSE);

  /////////////////////////////////////////////////////////////
  // start event loop 
  /////////////////////////////////////////////////////////////
  int n=0;
  int stat=0;
  int count = 1000;
  if(debug) count =1;
  TMemStat memory;

  while ( (stat==0 || stat==1 ) && n<nevents) 
  {
    chain->Clear();
    stat = chain->Make();
    if(n%count==0) 
    {
      cout << "Finished processing event number "<<n <<endl;
      memory.PrintMem(NULL);
    }
    n++;
  }
  cout << endl;
  chain->Finish();    
}

