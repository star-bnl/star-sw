// example macro to use  EEmcTTMMaker
// Author: Piotr A. Zolnierczuk
class StChain;
class StMuTrack;
class EEmcTower;
class TList;

StChain *chain=0;

void
ttm
(
 char* inpDir  = "/star/2003/mudst/",                    // MuDST directory
 char* inpFile = "st_physics_4145010_raw_*.MuDst.root",  // MuDST file(s)
 char* outFile = "R4145010.root",
 Int_t nFiles  = 50,                                     // # of MuDST file(s)
 Int_t nEvents = -1
 )
{ 
  gErrorIgnoreLevel=1999;
  cerr << "<xml>" << endl;

  // load root/root4star libraries
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  // load more libraries :)
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
  muDstMaker       = new StMuDstMaker(0,0,inpDir,inpFile,"MuDst.root",nFiles);  // muDST main chain
  StMuDbReader* db = StMuDbReader::instance();                                  // need the database
  StEEmcDbMaker  *eemcDbMaker=new StEEmcDbMaker("eemcDb");                      // need EEMC database  
  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");                // need the database (???)


  // now comment in/out/change the below if you want it your way
  eemcDbMaker->setSectors(5,8);            // request EEMC DB for sectors you need (dafault:1-12)
  eemcDbMaker->setTimeStampDay(20030514);  // format: yyyymmdd
  // eemcDbMaker->setDBname("TestScheme/eemc");               // use alternative database
  // eemcDbMaker->setPreferedFlavor("set430","eemcPMTcal");   // request alternative flavor of DB table (if needed)

  // finally after so many lines we arrive at the good stuff
  EEmcTTMMaker *ttm = new  EEmcTTMMaker ("TTM",muDstMaker,eemcDbMaker);
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
  cerr << "</xml>" << endl;
}
