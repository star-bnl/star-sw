
#include <TSystem.h>

// forward declarations
class StChain;
class StFgtDbMaker;
class StFgtRawDaqReader;
class StFgtRobustPedMaker;
class StFgtStatusMaker;
class StFgtPedStatQA;
class St_db_Maker;
class StDbConfigNode;
class StDbManager;
class StFgtGenPlotter;
class StFgtGenAVEMaker;
class StFgtGeneralBase;
class StFgtStraightTrackMaker;
class StFgtStraightPlotter;
class StFgtPointMaker;

StChain           *analysisChain = 0;
St_db_Maker       *dbMkr         = 0;
StFgtDbMaker      *fgtDbMkr      = 0; 
StFgtRawDaqReader *daqRdr        = 0;
StFgtGenPlotter    *fgtGenPlotter     = 0;
StFgtGenAVEMaker    *fgtAVEffMkr     = 0;
StFgtGeneralBase    *fgtGenBase     = 0;
StFgtStraightTrackMaker    *fgtStraightTracker     = 0;
/// /star/data03/daq/2012/064/13064033p_jb/st_physics_13064033_raw_1010001.daq
///star/data03/daq/2012/061/13061024fR/st_fgt_13061024_raw_1340001.daq",


int agvEffs( const Char_t *filenameIn = "/star/data03/daq/2012/174/13174038p_rf/st_physics_13174038_raw_1010001.daq",
	     Int_t nevents = 100, Int_t effDisk=20,
	     Bool_t cutShortEvents = 1 ){
   LoadLibs();   
   cout << "number of events: " << nevents <<endl;
   Int_t ierr = 0;

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("fgtEffAnalysisChain");

   std::string fgtDbMkrName = "";

   cout << "Loading St_db_Maker" << endl;
   gSystem->Load("libStDb_Tables.so");
   gSystem->Load("StDbLib.so");
   gSystem->Load("St_db_Maker");
   gSystem->Load("StDbBroker");


   TString dir0 = "MySQL:StarDb";
   TString dir1 = "$STAR/StarDb";
   St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );
   //dbMkr->SetDateTime(20120115,1);
   //for run 13061024, watch out for GMT!!
   //dbMkr->SetDateTime(20120301,162844);      // run 13025001 2012-01-25 08:03:34 GMT
   //run 13064033
   //   dbMkr->SetDateTime(20120304,173144);      // run 13025001 2012-01-25 08:03:34 GMT
   //for 1305459
   //  dbMkr->SetDateTime(20120223,201805);      
   //  dbMkr->SetDateTime(20120223,201805);      
   ///   dbMkr->SetDateTime(20120316,033657); ///
   //          dbMkr->SetDateTime(20120404,043459); ///
   //////--->for run12             dbMkr->SetDateTime(20120622,043459); ///
   //   dbMkr->SetDateTime(20120622,043459); ///
   ///            dbMkr->SetDateTime(20120622,043459); ///z
   //     dbMkr->SetDateTime(20120803,043459); ///for cosmic teststand
   //  dbMkr->SetDateTime(20120903,043459); ///for cosmic teststand

   ///     dbMkr->SetDateTime(20120307,000717); ///
   //      dbMkr->SetDateTime(20120128,204320);      // run ???
   //// begin of run 13   
   //   dbMkr->SetDateTime(20130301,043459); ///
   
   cout << "Constructing StFgtDbMaker" << endl;
   fgtDbMkr = new StFgtDbMaker( "fgtDb" );
   //fgtDbMkr->SetFlavor("ideal",""); // mapping is wrong, but at least the code runs...
   fgtDbMkrName = fgtDbMkr->GetName();
   
   //
   // NOW THE OTHER READERS AND MAKERS
   //
   cout << "Constructing the daq reader" << endl;
   daqRdr = new StFgtRawDaqReader( "daqReader", filenameIn, fgtDbMkrName.data() );
   daqRdr->setIsCosmic( false );
   //   daqRdr->setZSdataOnly();
   daqRdr->setNoneZSdataOnly();
   daqRdr->cutShortEvents( cutShortEvents );
   gSystem->Load("libStFgtA2CMaker");
   StFgtA2CMaker* a2cMkr  = new StFgtA2CMaker(  "FgtA2CMaker" );
   a2cMkr->setFgtDb(fgtDbMkr->getDbTables());
   a2cMkr ->setAbsThres( -5000 );  // set to below -4096 to skip cut
    //    a2cMkr ->setAbsThres( 300 );  // set to below -4096 to skip cut
   a2cMkr ->setRelThres( 4.);  // set to zero to skip cut
   //      a2cMkr->doCutBadStatus(true);//parameter is useless from looking at the function
   a2cMkr->doCutBadStatus();
   a2cMkr->acceptLongPulses(true);
   ////you have to set the relative threshold to 3 if you set the cluster threshold to 0.6 (meaning 3)
   a2cMkr->setClusterThreshold(0.8);

   ///this cuts ~10% of the events
   //   a2cMkr->doRemoveNonSignal(false);
   //   a2cMkr->doRemoveNonPulse(false);

   Char_t *myMaker = "StFgtClusterMaker";
  if (gClassTable->GetID(myMaker) < 0) {
	  gSystem->Load(myMaker);//  TString ts("load "; ts+=myMaker; StMemStat::PrintMem(ts.Data());
  }
  if (gClassTable->GetID("StFgtPointMaker") < 0) {
    gSystem->Load("StFgtPointMaker");//  TString ts("load "; ts+=myMaker; StMemStat::PrintMem(ts.Data());
  }
  StFgtClusterMaker* myMk =new StFgtClusterMaker("FgtClustMaker"); 
  //simplePointAlgo is default
  StFgtPointMaker* myPoMk =new StFgtPointMaker("FgtPointMaker"); 
  //  simpleClusAlgo = new StFgtSimpleClusterAlgo();
  seededClusAlgo = new StFgtSeededClusterAlgo();
  seededClusAlgo->setJumpSingleStrip(true); // if a strip in cluster has no charge 

  myMk->setClusterAlgo( seededClusAlgo );

  cout <<"1" <<endl;
  //  StFgtAVEfficiencyMaker* effMkr=new StFgtAVEfficiencyMaker("FgtAVEfficiencyMaker");
  //  StFgtClusterPlotter* clusPlot=new StFgtClusterPlotter("FgtClusterPlotter");
  //  fgtAVEffMkr = new StFgtGenAVEMaker( "avEffMkr" );
  fgtGenBase = new StFgtGeneralBase( "fgtGenBase" );
  fgtGenBase->fillFromEvent();

  fgtStraightTracker = new StFgtStraightTrackMaker( "fgtStraightTracker" );
  fgtStraightTracker->setMinNumFitPoints(3);
  fgtStraightTracker->SetEffDisk(effDisk);
  fgtStraightPlotter = new StFgtStraightPlotter( "fgtStraightPlotter" );
  //dca in cm for track associated clusters...
  fgtStraightPlotter->setDcaCut(2);

  //  fgtAVEffMkr->setChargeMatchCut(2.0);
  //  fgtAVEffMkr->setUseChargeMatch();
    //  fgtAVEffMkr->SetEffDisk(effDisk);
  //   fgtGenPlotter = new StFgtGenPlotter( "genPlotter" );

   // debug
   // analysisChain->ls(4);

   cout << "Initializing" << endl;
   ierr = analysisChain->Init();

   if( ierr ){
      cout << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1<<30; // a big number

   cout << "max nevents = " << nevents << endl;
   for( int i=0; i<nevents && !ierr; ++i ){
     if( i+1 % 100 == 1 )
         cout << "\ton event number **************" << i << endl;
     //	 cout << "clear (agv)" << endl;
	 analysisChain->Clear();
	 //      cout << "make" << endl;
	 ierr = analysisChain->Make();
	 //	 cout <<" done " <<endl;
   };

   //   fgtDbMkr->printFgtDumpCSV("fgtMapDump.csv");
   //
   // Calls the ::Finish() method on all makers
   //
   cout << "finish" << endl;
   analysisChain->Finish();

   // Now write a status table

   /*
      if( runnumber ){
         std::stringstream ss;
         fout << "Times given in the run log are " << endl;
         ss << "lynx -dump 'http://online.star.bnl.gov/RunLogRun12/index.php?r=" << runnumber << "' | grep GMT";
         FILE *f = gSystem->OpenPipe(ss.str().data(),"r");
         Char_t c;
         while((c=fgetc(f))!=EOF)
            fout << c;
	    };*/


   doOutputPdf=false;
   // convert ps to pdf
   if( doOutputPdf ){
      cout << "converting ps to pdf" << endl;
      gSystem->Exec(( std::string("ps2pdf -dAutoRotatePages=/None ") + pdfFile ).data());
   };

   cerr << "\tall done" << endl;
   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries
   gROOT->Macro("loadMuDst.C");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  //  gSystem->Load("StFgtUtil");
  gSystem->Load("StUtilities");
  gSystem->Load("StEvent");
  cout << "loaded StEvent library" << endl;

  gSystem->Load("RTS");
  gSystem->Load("StFgtUtil");
   gSystem->Load("StFgtDbMaker");
   gSystem->Load("StFgtClusterTools");
   //   gSystem->Load("StFgtPedPlotter");
   //  gSystem->Load("StFgtPool");
   //   gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtRawDaqReader");
  //   gSystem->Load("StFgtQaMakers");


};
