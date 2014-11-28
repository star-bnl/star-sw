/*
 * Author: S. Gliske (sgliske@anl.gov), March 2012
 *
 * Macro to run the simple text based occupancy QA maker
 * 
 */

// forward declarations
class StChain;
class StMuDstMaker;    
class StMuFgtOccTxtMkr;

// global variables
StChain            *analysisChain = 0;
StMuDstMaker       *muDstMaker    = 0;
StMuFgtOccTxtMkr   *occTxtMkr     = 0;

void runMuFgtOccTxtMkr( const Char_t *filename, 
                        Int_t neventsIn = 10 ){

   // load the shared libraries
   std::cout << "***** Loading libraries *****" << endl;
   LoadLibs();

   std::cout << "***** Done loading libraries *****" << endl;
   std::cout << "***** Instanciating all the classes *****" << endl;

   //
   // Create the analysis chain
   //
   analysisChain = new StChain("eemcAnalysisChain");

   //
   // MuDst maker for reading input
   //
   Int_t nfiles = 10000;
   muDstMaker = new StMuDstMaker( 0, 0, "", filename, "MuDst", nfiles );
   muDstMaker->SetStatus("*",0);
   muDstMaker->SetStatus("Event",1);
   muDstMaker->SetStatus("MuEvent",1);
   muDstMaker->SetStatus("PrimaryVertices",1);
   muDstMaker->SetStatus("FgtStrip",1);
   muDstMaker->SetStatus("FgtCluster",1);

   //
   // now the QA maker
   //
   occTxtMkr = new StMuFgtOccTxtMkr( "fgtOccTxtMkr" );


   // debugging info
   std::cout << "***** Done instanciating all the classes *****" << endl;
   //analysisChain.ls(3);

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
   gROOT->Macro("LoadLogger.C");

   // and a few others
   gSystem->Load("StDbLib");
   gSystem->Load("StDbBroker");
   gSystem->Load("St_db_Maker");
   gSystem->Load("StStarLogger");

   gSystem->Load("StFgtUtil");
   gSystem->Load("StMuFgtQa");
};

/*
 * $Id: runMuFgtOccTxtMkr.C,v 1.1 2012/03/06 01:32:35 sgliske Exp $
 * $Log: runMuFgtOccTxtMkr.C,v $
 * Revision 1.1  2012/03/06 01:32:35  sgliske
 * creation
 *
 */
