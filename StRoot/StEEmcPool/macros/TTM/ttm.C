// example macro to use  EETowTrackMatchMaker
// Author: Piotr A. Zolnierczuk
class StChain;
class StMuTrack;

StChain *chain=0;

void
ttm
(
 char* inpDir  = "/star/fy2003/mudst/",                        // MuDST directory
 char* inpFile = "st_physics_4145010_raw_0010001.MuDst.root",  // MuDST file(s)
 Int_t nFiles  = 1,                                            // # of MuDST file(s)
 Int_t nEvents = -1
 )
{ 
  gErrorIgnoreLevel=1999;

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
  EETowTrackMatchMaker *mm = new  EETowTrackMatchMaker ("TTM",muDstMaker,eemcDbMaker);
  mm->Summary(cout);    // 

  StMuDebug::setLevel(0);

  chain->Init();
  chain->ls(3);

  int stat=0;
  //---------------------------------------------------
  for(int counter=0; nEvents<0 || counter<nEvents ; ++counter) {
    if( (stat = chain->Make()) != 0 ) break;
    if(counter%100==0) cerr << "analyzed " << counter << " events" << endl;
  }

  mm->Summary(cout);    // 
}
