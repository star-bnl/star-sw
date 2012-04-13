/*
 * Author: S. Gliske (sgliske@anl.gov), March 2012
 *
 * Macro to run the simple text based occupancy QA maker and FGT
 * Helix-Hough tracking with input files in MuDSt format.
 * 
 */

// forward declarations
class StChain;
class StMuDstMaker;    
class St_db_Maker;
class StEEmcDbMaker;
class StEEmcRawMapMaker;
class StFgtLHTracking;
class StEEmcFgtLHTrackQa;

// global variables
StChain            *analysisChain = 0;
StMuDstMaker       *muDstMaker    = 0;
St_db_Maker        *starDatabase  = 0;
StEEmcDbMaker      *eemcDbMaker   = 0;
StEEmcRawMapMaker  *rawMapMkr     = 0;
StFgtLHTracking    *fgtTrkMkr     = 0;
StEEmcFgtLHTrackQa *fgtTrkQa      = 0;

void runFgtLHTracking_MuDst( const Char_t *filenameIn, 
                             const Char_t *filenameOut,
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
   // MuDst maker for reading input
   //
   Int_t nfiles = 10000;
   muDstMaker = new StMuDstMaker( 0, 0, "", filenameIn, "MuDst", nfiles );
   muDstMaker->SetStatus("*",0);
   muDstMaker->SetStatus("Event",1);
   muDstMaker->SetStatus("MuEvent",1);
   muDstMaker->SetStatus("PrimaryVertices",1);
   muDstMaker->SetStatus("FgtStrip",1);
   muDstMaker->SetStatus("FgtCluster",1);
   muDstMaker->SetStatus("EmcAll",1);

   //
   // Connect to the STAR databse
   //
   starDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");

   //
   // EEMC raw map maker
   //
   eemcDbMaker = new StEEmcDbMaker( "EEmcDbMkr" );
   rawMapMkr = new StEEmcRawMapMaker( "EEmcRawMapMaker" );
   rawMapMkr->setInput("MuDst",1);           // sets mudst as input

   //
   // the track maker
   //
   fgtTrkMkr = new StFgtLHTracking( "fgtTrkMkr" );
   fgtTrkMkr->setFitThres( 1 );     // cm
   fgtTrkMkr->setIncludeThres( 1 ); // cm
   fgtTrkMkr->setNumPoints( 3 ); // cm
   fgtTrkMkr->setNumAgreeThres( 1 ); // cm
   fgtTrkMkr->setUseVertex(1);

   //
   // QA Makers
   //
   fgtTrkQa = new StEEmcFgtLHTrackQa( "EEmcFgtLHTrackQa", "EEmcRawMapMaker", "fgtTrkMkr" );
   fgtTrkQa->setFileOutName( filenameOut );

   // debugging info
   std::cout << "***** Done instanciating all the classes *****" << endl;
   //analysisChain->ls(3);

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

   // and a few others
   gSystem->Load("StDbLib");
   gSystem->Load("StDbBroker");
   gSystem->Load("St_db_Maker");

   gSystem->Load("StFgtUtil");
   gSystem->Load("StMuFgtQa");
   gSystem->Load("StFgtTracking");
   gSystem->Load("StEEmcDbMaker");
   gSystem->Load("StEEmcUtil");
   gSystem->Load("StEEmcFgt");

};

/*
 * $Id: runFgtLHTracking_MuDst.C,v 1.3 2012/04/13 15:08:58 sgliske Exp $
 * $Log: runFgtLHTracking_MuDst.C,v $
 * Revision 1.3  2012/04/13 15:08:58  sgliske
 * updates
 *
 * Revision 1.2  2012/04/11 22:13:30  sgliske
 * update
 *
 * Revision 1.1  2012/04/09 16:14:23  sgliske
 * creation
 *
 *
 */
