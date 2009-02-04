// example macro to use  EEmcTTMMaker
// Author: Piotr A. Zolnierczuk, IUCF
// Date: 05/04/2004
// 

// ROOT/ROOT4STAR 
class StChain;
class StMuTrack;
class StMuDstMaker;
class StEventInfo;
// TTM STUFF
class EEmcTower;
class EEmcTTMatch;
class EEmcTTMMaker;

StChain       *chain  = 0;
EEmcTTMMaker  *ttmMk  = 0;
StMuDstMaker  *muDstMk= 0;

void
ttm
(
 char*  inpDir    = "",             // MuDST directory
 char*  inpFile   = "ttm.lis",      // MuDST file(s);                      
 char*  outFile   = "ttm.ndst.root",// output nano dst root file
 Int_t  nFiles    = 150,            // # of MuDST file(s)
 Int_t  nEvents   = -1		    // # of events
 )
  // NOTES: 
  // 1. StEEmcDbMaker has some limitations so beware of the following
  // ( complaints to appropriate autors) 
  // * StEEmcDbMaker works _only_ for single run , so make sure that
  //   your *.lis containf files for one run only, otherwise it will crash 
  //   like Windoza
// not true anymore - OG
  // 2. EEmcTTMMaker main "product" is a list of EEmcTTMatch'es which in turn 
  //   are EEmcTower plus a list of StMuTrack's that fullfill certain criteria. 
{ 
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
  
  // turn on profiling
  //gSystem->Setenv("JPROF_FLAGS", "JP_START JP_PERIOD=0.001"); 
  //gSystem->Load("libJprof"); 
  // % jprof `which root4star` jprof-log >jprof.html 

  // create the chain    
  chain = new StChain("StChain"); 
  
  // now we add Makers to the chain...  some of that is black magic to me :) 
  muDstMk = new StMuDstMaker(0,0,inpDir,inpFile,"",nFiles); // main chain
  StMuDbReader  *db       = StMuDbReader::instance();       // the database
  St_db_Maker   *dbMk     = new St_db_Maker("StarDb", "MySQL:StarDb"); // more?
  StEEmcDbMaker *eemcDbMk = new StEEmcDbMaker("eemcDb");    // EEMC database 


  // finally after so many lines we arrive at the good stuff
  ttmMk = new  EEmcTTMMaker ("TTM",muDstMk,eemcDbMk);
  ttmMk->SetFileName(outFile); // output nanoDst file
  // have cuts your way (optional)
  ttmMk->SetMaxCTBSum(1000); 
  ttmMk->SetMinTrackLength(20.0);
  ttmMk->SetMinTrackHits(5);
  ttmMk->SetMinTrackPt(0.5);
  ttmMk->SetMinTrackEta(0.7);
  ttmMk->SetMaxTrackEta(2.2);
  ttmMk->SetDeltaEtaCut(0.7); // ! note this is a fraction of tower width in eta
  ttmMk->SetDeltaPhiCut(0.7); // ! note this is a fraction of tower width in phi
  ttmMk->Summary(cout);       // prints cut summary

  StMuDebug::setLevel(0);
  chain->Init();
  chain->ls(3);

  // now comment in/out/change the below if you want it your way
  StEEmcDb *eemcDb = (StEEmcDb*)chain->GetDataSet("StEEmcDb");
  eemcDb->setPreferedFlavor("onlped","eemcPMTped"); // alternative flavor 

  //---------------------------------------------------
  int  stat = 0;
  int  event= 0;
  while(++event<=nEvents || nEvents<0) {
    stat=chain->Make();
    // STAR intelligence: stat=2 EOF,stat=4 FATAL; if so break the loop
    // if not OK (and not EOF nor FATAL) !!! try another event
    if( stat==2 || stat==4) break;
    if( stat!=0           ) continue;
    if(event%10 == 0 ) {
      cerr << "event " << event << "                     \r";
      cerr.flush();
    }
  }
  cerr << endl;
  ttmMk->Summary(cout);
}


