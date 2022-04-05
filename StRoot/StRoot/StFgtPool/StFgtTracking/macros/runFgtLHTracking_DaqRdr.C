/*
 * Author: S. Gliske (sgliske@anl.gov), March 2012
 *
 * Macro to run the simple text based occupancy QA maker and FGT
 * Helix-Hough tracking using the raw daq reader to read DAQ files.
 * 
 */

// forward declarations
class StChain;
class StFgtDbMaker;
class StFgtRawDaqReader;
class St_db_Maker;
class StDbConfigNode;
class StDbManager;
class StFgtLHTracking;


StChain           *analysisChain = 0;
St_db_Maker       *dbMkr         = 0;
StFgtDbMaker      *fgtDbMkr      = 0;
StFgtRawDaqReader *daqRdr        = 0;
StFgtLHTracking   *fgtTrkMkr     = 0;

void runFgtLHTracking_DaqRdr( const Char_t *filenameIn, 
                              Int_t date = 20120125,
                              Int_t time = 80350,       // run 13025001 2012-01-25 08:03:34 GMT
                              Int_t neventsIn = 10 ){

   // load the shared libraries
   std::cout << "***** Loading libraries *****" << endl;
   LoadLibs();

   std::cout << "***** Done loading libraries *****" << endl;
   std::cout << "***** Instanciating all the classes *****" << endl;

   //
   // Create the analysis chain
   //
   analysisChain = new StChain("fgtTrackingChain");

   //
   // Set up the date base
   //
   TString dir0 = "MySQL:StarDb";
   TString dir1 = "$STAR/StarDb";
   St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );

   dbMkr->SetDateTime(date,time);

   cout << "Loading StFgtDbMaker" << endl;
   gSystem->Load("StFgtDbMaker");

   cout << "Constructing StFgtDbMaker" << endl;
   fgtDbMkr = new StFgtDbMaker( "fgtDbMkr" );

   //
   // Raw DAQ reader
   //
   cout << "Constructing the daq reader" << endl;
   daqRdr = new StFgtRawDaqReader( "daqReader", filenameIn, "fgtDbMkr" );
   daqRdr->setIsCosmic( 0 );
   daqRdr->cutShortEvents( 1 );

   //
   // A2C Maker
   //
   cout << "Constructing the A2CMaker" << endl;
   StFgtA2CMaker* a2cMkr  = new StFgtA2CMaker( "FgtA2CMaker" );
   a2cMkr->setFgtDb( fgtDbMkr->getDbTables() );
   a2cMkr ->setAbsThres( -5000 );  // set to below -4096 to skip cut
   a2cMkr ->setRelThres( 4.);      // set to zero to skip cut
   a2cMkr->doCutBadStatus();       // 
   a2cMkr->doRemoveNonPulse(1);    //


   //
   // Cluster maker
   //
   cout << "Constructing the cluster maker" << endl;
   StFgtClusterMaker* fgtClusMkr = new StFgtClusterMaker("FgtClustMaker");
   StFgtSeededClusterAlgo* seededClusAlgo = new StFgtSeededClusterAlgo();
   fgtClusMkr->setClusterAlgo( seededClusAlgo );

   //
   // the track maker
   //
   fgtTrkMkr = new StFgtLHTracking( "fgtTrkMkr" );
   fgtTrkMkr->setFitThres( 1 );     // cm
   fgtTrkMkr->setIncludeThres( 1 ); // cm
   fgtTrkMkr->setNumPoints( 3 ); // cm
   fgtTrkMkr->setNumAgreeThres( 3 ); // cm


   // debugging info
   std::cout << "***** Done instanciating all the classes *****" << endl;
   analysisChain->ls(3);

   //
   // Initialize all makers
   //
   std::cout << "***** Initializing all makers in the analysis chain *****" << std::endl;

   analysisChain->Init();

   std::cout << "***** Initialization done *****" << std::endl;

   //
   // Finally ready to loop over the events 
   //

   // If nEvents is negative, reset to the maximum possible value
   // for an Int_t
   if( neventsIn < 0 )
      neventsIn = 1<<31-1;

   Int_t ierr  = kStOK;  // err flag
   Int_t nevents = 0;    // cumulative number of events in
   for( ; nevents < neventsIn && !ierr; ++nevents ){
      // clear
      analysisChain->Clear();

      // make
      ierr = analysisChain->Make();
   };
 
   //---------------------------------------------------------------


   //
   // Calls the ::Finish() method on all makers
   //
   analysisChain->Finish(); 

   //
   // Delete the chain
   //
   analysisChain->Delete();

   return;
};



// load the shared libraries
void LoadLibs() {
   // commong shared libraries
   gROOT->Macro("loadMuDst.C");
   //gROOT->Macro("LoadLogger.C");

   // and a few others
   gSystem->Load("libStDb_Tables.so");
   gSystem->Load("StDbLib");
   gSystem->Load("StDbBroker");
   gSystem->Load("St_db_Maker");
   gSystem->Load("StStarLogger");
   gSystem->Load("StFgtDbMaker");

   gSystem->Load("RTS");

   gSystem->Load("StFgtUtil");
   gSystem->Load("StFgtRawDaqReader");
   gSystem->Load("StFgtA2CMaker");
   gSystem->Load("StFgtClusterMaker");
   gSystem->Load("StFgtTracking");
};


/*
 * $Id: runFgtLHTracking_DaqRdr.C,v 1.1 2012/04/09 21:08:58 sgliske Exp $
 * $Log: runFgtLHTracking_DaqRdr.C,v $
 * Revision 1.1  2012/04/09 21:08:58  sgliske
 * creation
 *
 * Revision 1.1  2012/04/09 16:14:23  sgliske
 * creation
 *
 *
 */
