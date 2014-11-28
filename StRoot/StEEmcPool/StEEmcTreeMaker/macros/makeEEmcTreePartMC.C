/* 
 * Created Mar 2013, by S. Gliske
 *
 * Macro to write only McEEmcTree part of the EEmcTree
 * 
 */

// forward declarations
class StChain;
class StMuDstMaker;
class StMcEEmcTreeMaker_t;

//
// some variables that others tend to make global
//
StChain             *analysisChain    = 0;
StMuDstMaker        *muDstMaker       = 0;
StMcEEmcTreeMaker_t *mcTreeMakerPtr   = 0;

//
// the main routine
//
void makeEEmcTreePartMC( Long_t neventsIn = -1, 
                         const Char_t *inputFileName = "",
                         const Char_t *outputFileName = "McEEmcTree.root",
                         Int_t displayFreq = 1000 ){

   Bool_t isMC = 1;

   std::cout << "***** Loading libraries *****" << endl;

   gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
   loadSharedLibraries();
   assert( !gSystem->Load("StEEmcUtil"));
   assert( !gSystem->Load("StMcEvent"));
   assert( !gSystem->Load("StMcEventMaker"));
   assert( !gSystem->Load("StSpinDbMaker") );
   assert( !gSystem->Load("StEEmcPoolEEmcTreeContainers") );
   assert( !gSystem->Load("StEEmcPointMap") );
   assert( !gSystem->Load("StEEmcTreeMaker") );

   //gDebug=5;

   std::cout << "***** Done loading libraries *****" << endl;
   std::cout << "***** Instanciating all the classes *****" << endl;

   //
   // Create the analysis chain
   //
   analysisChain = new StChain("eemcAnalysisChain");

   //
   // IO Maker to read the geant file
   //
   if( isMC ){
      ioMaker = new StIOMaker();

      TString geantFileName = inputFileName;
      geantFileName.ReplaceAll("MuDst","geant");
      ioMaker->SetFile(geantFileName);

      ioMaker->SetIOMode("r");
      ioMaker->SetBranch("*",0,"1");             //deactivate all branches
      ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
      ioMaker->SetBranch("minimcBranch",0,"r");   //activate geant Branch
   };

   //
   // MuDst maker for reading input
   //
   Int_t nfiles = 10000;
   muDstMaker = new StMuDstMaker( 0, 0, "", inputFileName, "MuDst", nfiles );
   muDstMaker->SetStatus("*",0);
   muDstMaker->SetStatus("*Event*",1);
   muDstMaker->SetStatus("PrimaryVertices",1);
   muDstMaker->SetStatus("EmcAll",1);

   //
   // Maker for StMcEvent
   //
   StMcEventMaker *mcEventMaker = new StMcEventMaker();
   mcEventMaker->doPrintEventInfo = false;
   mcEventMaker->doPrintMemoryInfo = false;

   // this is the interface to the STAR logger
   // SwitchOn("D") turns on debugging
   // SwitchOff("D") turns off debuggin
   // "I" is info and "W" is warn
   gMessMgr->SwitchOff("D");
   gMessMgr->SwitchOff("I");
   //gMessMgr->SetLevel(2);

   // McTree Maker 
   mcTreeMakerPtr = new StMcEEmcTreeMaker_t( "McEEmcTreeMkr" );
   mcTreeMakerPtr->setEnergyThreshold( 0.0 );
   mcTreeMakerPtr->addTrigger( -999 );
   mcTreeMakerPtr->setTreeStatus( StMcEEmcTreeMaker_t::WRITE, outputFileName );

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

   // If neventsIn/Out is negative, reset to a large value
   // for an Int_t
   if( neventsIn < 0 )
      neventsIn = 1<<30-1;

   Int_t ierr  = kStOK;  // err flag
   Long_t nevents = 1;    // cumulative number of events in
   for( ; nevents <= neventsIn && !ierr; ++nevents ){
      // clear
      analysisChain->Clear();

      // make
      ierr = analysisChain->Make();

      // Print every so many events
      if( (nevents+1) % displayFreq == 1 )
         std::cout << "***** finished event number " << nevents << " *****" << std::endl;

      if( ierr )
         std::cout << "***** ERROR FLAG " << ierr << " on event number " << nevents << " *****" << endl;
   };
 
   //---------------------------------------------------------------


   //
   // Calls the ::Finish() method on all makers
   //
   analysisChain->Finish(); 

   //
   // Delete the chain
   //
   // analysisChain->Delete();

   return;
};

/*
 * $Id: makeEEmcTreePartMC.C,v 1.1 2013/03/19 18:48:46 sgliske Exp $
 * $Log: makeEEmcTreePartMC.C,v $
 * Revision 1.1  2013/03/19 18:48:46  sgliske
 * creation
 *
 *
 */
