////////////////////////////////////////////////////////////////////
//
//                 pi0 analysis in dAu for each tower
//
////////////////////////////////////////////////////////////////////
class StChain;
StChain *chain=0;

void doPi0dAu(Int_t nevents=10, Int_t nFiles=10, const char* fileList="./FileList/UPCCombined.1.lis", const char* outFile="Output.root")
{
  cout << "fileList " <<fileList<<endl;  
  cout << "nevents: " <<nevents<<endl;
  cout << "nFiles:  " <<nFiles<<endl;  
  
  int debug=0; 
  
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libgen_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StMagF");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  
  // libraries for the DB
  gSystem->Load("StDaqLib");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_db_Maker"); 	
 
  gSystem->Load("StEmcADCtoEMaker");		
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
// Load my maker
  gSystem->Load("StPi0PeakMaker");
  cout << "\n Finished loading libraries " << endl;

//  gROOT->ProcessLine(".T");
  
  TMemStat memory("test");

// create the chain    
  chain = new StChain("chain"); 
  if (debug==1) chain->SetDebug(1);

  StMuDebug::setLevel(0);       
// Now we add Makers to the chain...
  maker = new StMuDstMaker(0,0,"",fileList,"",nFiles); // set up maker in read mode
  StMuDbReader* db = StMuDbReader::instance();
  
  // convert to StEvent
  StMuDst2StEventMaker *m = new StMuDst2StEventMaker();
  
  // Connect software to the database
  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");       	
  // set Timestamp
  // new MIP+electron calibration
  dbMk->SetMaxEntryTime(20021114,000200);
  // for eta bin calibration
  //dbMk->SetMaxEntryTime(20031101,000000);
  
  StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
  StPreEclMaker *pre    = new StPreEclMaker();
  StEpcMaker *epc       = new StEpcMaker();
  adc->setPrint(kFALSE);
  pre->setPrint(kFALSE);
  epc->setPrint(kFALSE);

  // my maker 
  StPi0PeakMaker *pi = new StPi0PeakMaker("pi0PeakMaker",kTRUE,outFile);

// now execute the chain Init functions
  if (debug==1) chain->PrintInfo();
  //Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  //if (initStat) chain->Fatal(initStat, "during Init()");
  chain->Init();
  
  Int_t evt=0, istat=0; 
  
  while ((istat==0 || istat==1 || istat==2) && evt<nevents)
  {
     chain->Clear();
     cout << "\t\t ---------- Processing event: " << evt+1 <<" ----------" <<endl;
     istat = chain->Make();
     
     memory.PrintMem("event loop: ");
     
     evt++;
  }
  chain->Finish();
}

