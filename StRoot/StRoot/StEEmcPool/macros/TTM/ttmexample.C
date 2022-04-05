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
ttmexample
(
 char*  inpDir    = "",             // MuDST directory
 char*  inpFile   = "ttm.lis",      // MuDST file(s);                      
 char*  outFile   = "ttm.ndst.root",// output nano dst root file
 Int_t  nFiles    = 150,            // # of MuDST file(s)
 Int_t  nEvents   = -1              // # of events
 )
  // NOTES: 
  // 1. EEmcTTMMaker main "product" is a list of EEmcTTMatch'es which in turn are 
  //    EEmcTower plus a list of StMuTrack's that fullfill certain criteria. 
  // 2. Optionally the Maker created a nanoDst file [ or a 'Not-a-Dst' file :) ]
  // 
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

  // create the chain    
  chain = new StChain("StChain"); 
  
  // now we add Makers to the chain...  some of that is black magic to me :) 
  muDstMaker = new StMuDstMaker(0,0,inpDir,inpFile,"",nFiles);  // muDST main chain
  StMuDbReader  *db          = StMuDbReader::instance();        // need the database
  St_db_Maker   *dbMk        = new St_db_Maker("StarDb", "MySQL:StarDb"); // need more db?
  StEEmcDbMaker *eemcDbMaker = new StEEmcDbMaker("eemcDb");     // need EEMC database  

  // now comment in/out/change the below if you want it your way
  eemcDbMaker->setSectors(1,12);           // request sectors you need (default:1-12)
  eemcDbMaker->setPreferedFlavor("onlped","eemcPMTped"); // request alternative db flavor 

  // finally after so many lines we arrive at the good stuff
  ttm = new  EEmcTTMMaker ("TTM",muDstMaker,eemcDbMaker);
  ttm->SetFileName(outFile); // output nanoDst file
  // have cuts your way (optional)
  ttm->SetMaxCTBSum(1000); 
  ttm->SetMinTrackLength(20.0);
  ttm->SetMinTrackHits(5);
  ttm->SetMinTrackPt(0.5);
  ttm->SetMinTrackEta(0.7);
  ttm->SetMaxTrackEta(2.2);
  ttm->SetDeltaEtaCut(0.7); // ! note this is a fraction of tower width in eta
  ttm->SetDeltaPhiCut(0.7); // ! note this is a fraction of tower width in phi
  // this is even more optional :)
  // the lines here repeat the default
  // ttm->ResetZPositionsArray();
  // ttm->AddZPosition("pres",kEEmcZPRE1+0.1);
  // ttm->AddZPosition("post",kEEmcZPOST-0.1);
  // ttm->AddZPosition("smd" ,kEEmcZSMD);
  ttm->Summary(cout);       // prints cut summary

  StMuDebug::setLevel(0);
  chain->Init();
  chain->ls(3);

  //---------------------------------------------------
  int  stat=0;
  int  event=0;
  while(++event<nEvents) {
    stat=chain->Make();
    // STAR intelligence: stat=2 EOF,stat=4 FATAL; if so break the loop
    // if not OK (and not EOF nor FATAL) !!! try another event
    if( stat==2 || stat==4) break;
    if( stat!=0           ) continue;

    // if no track to tower matches try another event
    if(ttm->GetMatchList()->IsEmpty()) continue;

    // set up iterator and pointers
    TIter  nextMatch(ttm->GetMatchList());
    EEmcTTMatch *tmatch;
    EEmcTower   *tower;
    StMuTrack   *track;
    //event info (for fun), it shows we like xml
    StEventInfo    &evInfo = muDstMaker->muDst()->event()->eventInfo();   
    cerr << "<Event";
    cerr << "Run=\""  << evInfo.runId() << "\"\t";
    cerr << "Event=\""<< evInfo.id()    << "\">\n";
    // loop over all towers with track hits
    while ((tmatch = (EEmcTTMatch*) nextMatch())) {
      tmatch->Out(cerr); // prints all match info
      tower = tmatch->Tower();
      // here's how to acces tower information
      const char *tLabel = tower->TowerLabel();
      int sector = tower->Sec();    // 0..11
      int subsec = tower->SubSec(); // 0..4
      int etabin = tower->Eta();    // 0..11
      float adc  = tower->ADC();    // adc - pedestal
      float de   = tower->dE();     // (adc - pedestal)/gain
      //
      int seclab = tower->SecLabel();    // 1..12
      int sublab = tower->SubSecLabel(); // A..E
      int etalab = tower->EtaLabel();    // 1..12
      // now more than one track may hit a tower
      TIter nextTrack(tmatch->Tracks());
      while((track=(StMuTrack *)nextTrack()))  {
	// how to access StMuTrack consult muDST manual (does not exist)
	TVector3 r;
	// for example one could extrapolate track to a given z-depth
	EEmcTTMatch::ExtrapolateToZ(track,290.0,r);
	double pt = track->pt();
        double x  = r.x();
      }
    }
    cerr << "</Event>" << endl;
  }
  ttm->Summary(cerr);
}


