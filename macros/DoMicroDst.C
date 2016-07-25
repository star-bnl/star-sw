  
////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

void DoMicroDst(bool  debug   = true,
                   char* dir    = "/star/data42/reco/production62GeV/ReversedFullField/P04id/2004/090/",
                   int   nFiles  = 10,
                   int   nevents = 1000,
                  )
{
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");

// Load my maker

// create chain    
  chain = new StChain("StChain"); 
  if(debug) chain->SetDebug();
   
// Now we add Makers to the chain...
   
  maker = new StMuDstMaker(0,0,dir,"","MuDst.root",nFiles);
  StMuDbReader* db = StMuDbReader::instance();

  StMuDst2StEventMaker *m = new StMuDst2StEventMaker();
  
  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");

  StEmcADCtoEMaker *adc=new StEmcADCtoEMaker();
  
  chain->Init();  

  int n=0;
  int stat=0;
  int count = 2000;
  if(debug) count =1;
  TMemStat memory;

  while ( (stat==0 || stat==1 || stat==2) && n<nevents) {
    chain->Clear();
    stat = chain->Make();
    if(n%count==0) 
    {
      cout << "Finished processing event number "<<n <<endl;
      memory.PrintMem(NULL);
      //memory.Summary();      
    }
    n++;
  }
  cout << endl;
  chain->Finish();
}

