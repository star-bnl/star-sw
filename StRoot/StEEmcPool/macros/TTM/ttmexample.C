// example macro to use  EEmcTTMMaker
// Author: Piotr A. Zolnierczuk

class StPhysicalHelixD;
class StThreeVectorD;

class StChain;
class StMuTrack;
class StMuDstMaker;
class StEventInfo;
class StEventSummary;


class EEmcTower;
class EEmcTTMatch;
class EEmcTTMMaker;


StChain       *chain=0;
EEmcTTMMaker  *ttm  =0;
StMuDstMaker  *muDstMaker= 0;



void
ttmexample
(
 char* inpDir  = "",         // MuDST directory
 char* inpFile = "show.lis", // MuDST file(s);                      
 char* outFile = "show.root",// output tree file
 Int_t nFiles  = 50,         // # of MuDST file(s)
 Int_t nEvents = 100         // # of events
 )
  // remeber to adjust dbase timestamp below !!!! 
  // what a ... design
{ 
  //gErrorIgnoreLevel=1999;

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
  //

  
  // now we add Makers to the chain...  some of that is black magic to me :) 
  muDstMaker = new StMuDstMaker(0,0,inpDir,inpFile,"",nFiles);  // muDST main chain
  StMuDbReader  *db          = StMuDbReader::instance();                        // need the database
  StEEmcDbMaker *eemcDbMaker =new StEEmcDbMaker("eemcDb");                      // need EEMC database  
  St_db_Maker   *dbMk        = new St_db_Maker("StarDb", "MySQL:StarDb");       // need the database (???)

  // now comment in/out/change the below if you want it your way
  eemcDbMaker->setSectors(1,12);            // request EEMC DB for sectors you need (dafault:1-12)
  eemcDbMaker->setTimeStampDay(20040331);  // format: yyyymmdd
  eemcDbMaker->setPreferedFlavor("onlped","eemcPMTped"); // request alternative flavor (if needed)

  // finally after so many lines we arrive at the good stuff
  ttm = new  EEmcTTMMaker ("TTM",muDstMaker,eemcDbMaker);
  ttm->SetFileName(outFile);
  ttm->Summary(cout);    // 
  ttm->SetMaxCTBSum(1000);
  ttm->SetMinTrackLength(20.0);
  ttm->SetMinTrackHits(5);
  ttm->SetMinTrackPt(0.5);
  ttm->SetMinTrackEta(0.7);
  ttm->SetMaxTrackEta(2.2);
  ttm->SetDeltaEtaCut(0.7); // ! note this is a fraction of tower width in eta
  ttm->SetDeltaPhiCut(0.7); // ! note this is a fraction of tower width in phi

  StMuDebug::setLevel(0);

  chain->Init();
  chain->ls(3);

  //---------------------------------------------------
  int  stat=0;
  int  event=0;
  while(event<nEvents) {
    stat=chain->Make();
    if( stat==2 || stat==4) break;
    if( stat!=0           ) continue;

    if(ttm->GetMatchList()->IsEmpty()) continue;

    TIter  nextMatch(ttm->GetMatchList());
    
    EEmcTTMatch *tmatch;
    EEmcTower   *tower;
    StMuTrack   *track;
    //
    event++;
    //
    StEventInfo    &evinfo = muDstMaker->muDst()->event()->eventInfo();   // event info
    StEventSummary &evsumm = muDstMaker->muDst()->event()->eventSummary();// event summary
    //
    cerr << "<Event";
    cerr << "Run=\""  << evinfo.runId() << "\"\t";
    cerr << "Event=\""<< evinfo.id()    << "\">\n";
    while ((tmatch = (EEmcTTMatch*) nextMatch())) {
      tmatch->Out(cerr);
    }
    cerr << "</Event>" << endl;
  }
  ttm->Summary(cerr);
}






