// example macro to use  EEmcTTMMaker
// Author: Piotr A. Zolnierczuk
class StChain;
class StMuDstMaker;
class EEmcTTMMaker;

StChain      *chain     =0;
StMuDstMaker *muDstMaker=0;
EEmcTTMMaker *ttm       =0;
 
void loadSharedLibraries();

void
ttm
(
 char* inpDir  = "/star/2004/mudst/",    // MuDST directory
 char* inpFile = "R5086033b.MuDst.root", // MuDST file(s)
 char* outFile = "x.root",
 Int_t nFiles  = 1,                      // # of MuDST file(s)
 Int_t nEvents = 50000
 )
{ 
  gErrorIgnoreLevel=1999;
  cerr << "<xml>" << endl;

  // load root/root4star libraries
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  // load more libraries :)
  gSystem->Load("libmysqlclient");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");

  // load even more libraries (EEMC stuff) 
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcPoolTTM");

  // create the chain    
  chain = new StChain("StChain"); 
  
  // now we add Makers to the chain...  some of that is black magic :) 
  muDstMaker  = new StMuDstMaker(0,0,inpDir,inpFile,"MuDst.root",nFiles);  // muDST main chain
  StMuDbReader   *db         = StMuDbReader::instance();                   // need the database
  StEEmcDbMaker  *eemcDbMaker=new StEEmcDbMaker("eemcDb");                 // need EEMC database  
  St_db_Maker    *dbMk       = new St_db_Maker("StarDb", "MySQL:StarDb");  // need the database (???)

  // now comment in/out/change the below if you want it your way
  //eemcDbMaker->setSectors(5,8);            // request EEMC DB for sectors you need (dafault:1-12)
  eemcDbMaker->setSectors(1,12);            // request EEMC DB for sectors you need (dafault:1-12)
  eemcDbMaker->setTimeStampDay(20040327);  // format: yyyymmdd
  eemcDbMaker->setPreferedFlavor("onlped","eemcPMTped"); // request alternative flavor (if needed)
  // eemcDbMaker->setTimeStampDay(20030514);  // format: yyyymmdd
  // eemcDbMaker->setDBname("TestScheme/eemc");               // use alternative database   (if needed)
  // eemcDbMaker->setPreferedFlavor("set430","eemcPMTcal");   // request alternative flavor (   -//-  )

  // finally after so many lines we arrive at the good stuff
  ttm = new  EEmcTTMMaker ("TTM",muDstMaker,eemcDbMaker);
  ttm->SetFileName(outFile);
  ttm->Summary(cerr);    // 

  StMuDebug::setLevel(0);

  chain->Init();
  chain->ls(3);

  int stat=0;
  int counter=0;
  //---------------------------------------------------
  while(nEvents<0 || counter<nEvents ) {
    if( (stat = chain->Make()) != 0 ) break;

    // just for fun
    switch(counter%4) {
    case  0: cout << "\\\r"; break; 
    case  1: cout << "|\r"; break; 
    case  2: cout << "-\r"; break; 
    case  3: cout << "/\r"; break; 
    default: cout << ".\r"; break; 
    }
    if(++counter%100==0) cout << "analyzed " << counter << " events" << endl;
    cout.flush();
  }
  ttm->Summary(cerr);    // 
  ttm->Finish();
  cerr << "</xml>" << endl;
}
