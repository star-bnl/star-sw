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

StChain       *chain=0;
EEmcTTMMaker  *ttm  =0;
StMuDstMaker  *muDstMaker= 0;

void
ttm
(
 char*  inpDir    = "",                // MuDST directory
 char*  inpFile   = "ttm093.lis",      // MuDST file(s);                      
 char*  outFile   = "ttm093.ndst.root",// output nano dst root file
 Int_t  nFiles    = 75,                // # of MuDST file(s)
 Int_t  nEvents   = -1,                // # of events
 Int_t timeStamp  = 20040331           // format: yyyymmdd
 )
  // NOTES: 
  // 1. StEEmcDbMaker has some limitations so beware of the following
  // ( complaints to appropriate autors) 
  // * StEEmcDbMaker works _only_ for single run , so make sure that
  //   your *.lis containf files for one run only, otherwise it will crash like Windoza
  // * remember to adjust dbase timestamp above to match your runs as iStEEmcDbMaker 
  //   is unable (as of Apr 2004) to read timestamp off a muDST file, what a .....
  // 2. EEmcTTMMaker main "product" is a list of EEmcTTMatch'es which in turn are 
  //   EEmcTower plus a list of StMuTrack's that fullfill certain criteria. 
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
  muDstMaker = new StMuDstMaker(0,0,inpDir,inpFile,"",nFiles);  // muDST main chain
  StMuDbReader  *db          = StMuDbReader::instance();        // need the database
  StEEmcDbMaker *eemcDbMaker = new StEEmcDbMaker("eemcDb");     // need EEMC database  
  St_db_Maker   *dbMk        = new St_db_Maker("StarDb", "MySQL:StarDb"); // need more db?

  // now comment in/out/change the below if you want it your way
  eemcDbMaker->setSectors(1,12);           // request sectors you need (default:1-12)
  eemcDbMaker->setTimeStampDay(timeStamp); // format: yyyymmdd
  eemcDbMaker->setPreferedFlavor("onlped","eemcPMTped"); // request alternative db flavor 

  // finally after so many lines we arrive at the good stuff
  ttm = new  EEmcTTMMaker ("TTM",muDstMaker,eemcDbMaker);
  ttm->SetFileName(outFile);
  //ttm->WriteTree(false);
  // have cuts your way (optional)
  ttm->SetMaxCTBSum(1000); 
  ttm->SetMinTrackLength(20.0);
  ttm->SetMinTrackHits(5);
  ttm->SetMinTrackPt(0.5);
  ttm->SetMinTrackEta(0.7);
  ttm->SetMaxTrackEta(2.2);
  ttm->SetDeltaEtaCut(0.7); // ! note this is a fraction of tower width in eta
  ttm->SetDeltaPhiCut(0.7); // ! note this is a fraction of tower width in phi
  ttm->Summary(cout);       // prints cut summary

  StMuDebug::setLevel(0);
  chain->Init();
  chain->ls(3);

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
  ttm->Summary(cerr);
}


