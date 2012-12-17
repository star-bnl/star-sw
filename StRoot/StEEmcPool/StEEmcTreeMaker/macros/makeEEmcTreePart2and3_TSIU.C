/* 
 * Created May 2012, by S. Gliske
 *
 * Macro to read in "Part 1" of the EEmcTree and write out "Part 2"
 * and "Part 3" using the TSIU algorithm.
 * 
 */

// forward declarations
class StChain;

// algos
class StEEmcHitMakerSimple_t;
class StEEmcTowerClusterFinder_t;
//class StEEmcTowerClusterFinderMinesweeper_t;
class StEEmcStripClusterFinder_t;
class StEEmcStripClusterFinderTSIU_t;
class StEEmcPointFinder_t;
class StEEmcPointFinderIU_t;
class StEEmcEnergyApportioner_t;
class StEEmcEnergyApportionerIU_t;

// other
class StEEmcTreeMaker_t;

//
// some variables that tend to be made global
//
StChain                                 *analysisChain = 0;
StEEmcTowerClusterFinder_t              *towerClusterFinderPtr = 0;
StEEmcStripClusterFinderTSIU_t          *stripClusterFinderPtr = 0;
StEEmcPointFinderIU_t                   *pointFinderPtr        = 0;
StEEmcEnergyApportionerIU_t             *energyApportionerPtr  = 0;
StEEmcHitMakerSimple_t                  *hitMakerPtr           = 0;
StEEmcTreeMaker_t                       *treeReaderPtr         = 0;
StEEmcTreeMaker_t                       *treeWriterPtr         = 0;

//
// the main routine
//
void makeEEmcTreePart2and3_TSIU( const Char_t *eemcTreePart1FileName = "eemcTreeP1.root",
                                const Char_t *eemcTreePart2FileName = "eemcTreeP2.root",
                                const Char_t *eemcTreePart3FileName = "eemcTreeP3.root",
                                Int_t neventsIn = -1,
                                Bool_t isMC = 0,
                                Int_t displayFreq = 100 ){

   // load the shared libraries
   std::cout << "***** Loading libraries *****" << endl;
   loadEEmcTreeLibs();

   std::cout << "***** Done loading libraries *****" << endl;
   std::cout << "***** Instanciating all the classes *****" << endl;

   //
   // CREATE THE ANALYSIS CHAIN
   //
   analysisChain = new StChain("eemcAnalysisChain");

   //
   // TREE MAKER FOR READING
   //
   treeReaderPtr = new StEEmcTreeMaker_t( "EEmcTreeReader" );
   treeReaderPtr->setTreeStatus( StEEmcTreeMaker_t::PART_1, StEEmcTreeMaker_t::READ,   eemcTreePart1FileName );
   treeReaderPtr->setTreeStatus( StEEmcTreeMaker_t::PART_2, StEEmcTreeMaker_t::IGNORE, "" );
   treeReaderPtr->setTreeStatus( StEEmcTreeMaker_t::PART_3, StEEmcTreeMaker_t::IGNORE, "" );
   treeReaderPtr->doSpinInfoIO( !isMC );
   treeReaderPtr->doEvtHddrIO( 1 );
   treeReaderPtr->setMaxNumEvents( neventsIn );


   //
   // CREATE ALL THE FINDERS AND THE HIT MAKER
   //

   // tower cluster finder
//    towerClusterFinderPtr = new StEEmcTowerClusterFinderMinesweeper_t();
//    towerClusterFinderPtr->setSeedEnergyThreshold( 2.0 );

   // strip cluster finder
   stripClusterFinderPtr = new StEEmcStripClusterFinderTSIU_t();

   // parameter set d

   stripClusterFinderPtr->setNumSmoothIters( 10 );
   stripClusterFinderPtr->setNumStripsPerSide( 3 );
   stripClusterFinderPtr->setMinStripsPerCluster( 5 );
   stripClusterFinderPtr->setSeedAbsThres( 0.002 );
   stripClusterFinderPtr->setSeedRelThres( 0.0 );
   stripClusterFinderPtr->setMinEnergyPerCluster( 0.003 );

   // point finder
   pointFinderPtr = new StEEmcPointFinderIU_t();

   // energy apportioner
   energyApportionerPtr = new StEEmcEnergyApportionerIU_t();
   energyApportionerPtr->setCheckTowerBits(0);

   // Hit maker
   hitMakerPtr = new StEEmcHitMakerSimple_t ( "hitMaker",
                                              "EEmcTreeReader",
                                              towerClusterFinderPtr,
                                              stripClusterFinderPtr,
                                              pointFinderPtr,
                                              energyApportionerPtr
                                              );
   hitMakerPtr->doClusterTowers( 0 );
   hitMakerPtr->doClusterPreShower1( 0 );
   hitMakerPtr->doClusterPreShower2( 0 );
   hitMakerPtr->doClusterPostShower( 0 );
   hitMakerPtr->doClusterSMDStrips( 1 );

   //
   // Extra things if MC
   //

   // Associate hits with tracks, if it is MC data
   //    if( isMC )
   //       mcHitMakerPtr = new StMcEEmcHitMakerStrips_t( "mcHitMaker", "responseTreeReader", "hitMaker" );

   //
   // TREE MAKER FOR WRITING
   //
   treeWriterPtr = new StEEmcTreeMaker_t( "EEmcTreeWriter" );
   treeWriterPtr->setTreeStatus( StEEmcTreeMaker_t::PART_1, StEEmcTreeMaker_t::IGNORE, "" );
   treeWriterPtr->setTreeStatus( StEEmcTreeMaker_t::PART_2, StEEmcTreeMaker_t::WRITE,  eemcTreePart2FileName );
   treeWriterPtr->setTreeStatus( StEEmcTreeMaker_t::PART_3, StEEmcTreeMaker_t::WRITE,  eemcTreePart3FileName );
   treeWriterPtr->doSpinInfoIO( 0 );
   treeWriterPtr->doEvtHddrIO( 0 );
   treeWriterPtr->doMakePairs( 1 );
   treeWriterPtr->setEEmcTreeReader( treeReaderPtr );
   treeWriterPtr->setEEmcHitMkr( hitMakerPtr );

   // debugging info
   std::cout << "***** Done instanciating all the classes *****" << endl;
   //analysisChain->ls(3);

   //
   // INITIALIZE ALL MAKERS
   //

   std::cout << "***** Initializing all makers in the analysis chain *****" << std::endl;

   analysisChain->Init();

   std::cout << "***** Initialization done *****" << std::endl;

   //
   // FINALLY READY TO LOOP OVER THE EVENTS 
   //

   Int_t ierr  = kStOK;  // err flag
   Int_t nIn = -1;
   if( neventsIn < 0 )
      neventsIn = 1<<30;

   Int_t nhits = 0;

   for( nIn = 0; nIn < neventsIn && !ierr; ++nIn ){

      // clear
      analysisChain->Clear();

      // make
      ierr = analysisChain->Make();

      // number of hits
      nhits += hitMakerPtr->getHitVecSize();

      // Print every so many events
      if( (nIn+1) % displayFreq == 0 )
         std::cout << "***** Entries read " << nIn+1 << ", total hits = " << nhits << endl;
   };

   std::cout << "***** Entries read " << nIn+1 << ", total hits = " << nhits << endl;

   if( ierr && ierr != 2 )
      std::cout << "***** ERROR FLAG " << ierr << endl;
 
   //---------------------------------------------------------------
   //
   // Calls the ::Finish() method on all makers (done automatically)
   //
   // analysisChain->Finish(); 

   //
   // Delete the chain (done automatically)
   //
   //analysisChain->Delete();
   //delete analysisChain;

   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries
   gROOT->Macro("loadMuDst.C");
   gROOT->Macro("LoadLogger.C");

   // and a few others
   gSystem->Load("StDbLib");
   gSystem->Load("StDbBroker");
   gSystem->Load("St_db_Maker");
   gSystem->Load("StSpinDbMaker");
   gSystem->Load("StEEmcUtil");
   gSystem->Load("StStarLogger");

   gSystem->Load("StEEmcA2EMaker");
   gSystem->Load("StSpinInfoMaker");
   gSystem->Load("StEEmcResponseTreeMaker");
   gSystem->Load("StEEmcHitMaker");
   gSystem->Load("StEEmcViewer");
   gSystem->Load("StEEmcTreeMaker_t");
   gSystem->Load("StEEmcPi0Maker");
   gSystem->Load("libSpectrum");

};

// $Id: makeEEmcTreePart2and3_TSIU.C,v 1.1 2012/12/17 20:01:38 sgliske Exp $
// $Log: makeEEmcTreePart2and3_TSIU.C,v $
// Revision 1.1  2012/12/17 20:01:38  sgliske
// moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker/macros
//
// Revision 1.1  2012/09/04 19:53:15  sgliske
// first added to CVS
//
// Revision 1.2  2012/05/31 22:13:14  sgliske
// working on MC
//
// Revision 1.1  2012/05/25 16:20:19  sgliske
// ready for production (I hope)
//
// Revision 1.2  2012/05/25 04:21:57  sgliske
// updates
//
// Revision 1.1  2012/05/22 22:01:08  sgliske
// updates
//
// Revision 1.8  2011/10/24 21:49:40  sgliske
// updates, working on reading MC code
//


// forward declarations
class StChain;
class St_db_Maker;
class StEEmcDbMaker;
class StMuDstMaker;
class StEEmcA2EMaker;
class StSpinDbMaker;
class StEEmcSlowMaker;

class StSpinInfoMaker_t;
class StEEmcEnergyMaker_t;
class StEEmcTreeMaker_t;

//
// some variables that others tend to make global
//
StChain             *analysisChain    = 0;
StMuDstMaker        *muDstMaker       = 0;
St_db_Maker         *starDatabase     = 0;
StEEmcDbMaker       *eemcDbMaker      = 0;
StSpinDbMaker       *spinDb           = 0;
StEEmcA2EMaker      *a2EMakerPtr      = 0;
StSpinInfoMaker_t   *spinInfoMakerPtr = 0;
StEEmcEnergyMaker_t *energyMakerPtr   = 0;
StEEmcTreeMaker_t   *treeWriterPtr     = 0;

//
// the main routine
//
void makeEEmcTreePart1( Long_t neventsIn = -1, 
                        Long_t neventsOut = -1, 
                        const Char_t *inputFileName = "",
                        const Char_t *outputFileName = "eemcTreeP1.root",
                        Int_t displayFreq = 100,
                        Int_t trigID = 137641,
                        Int_t doSpinIO = 1,
                        Bool_t isMC = 0 ){

   std::cout << "***** Loading libraries *****" << endl;
   gROOT->Macro("LoadEEmcTreeLibs.C");

   //gDebug=5;

   if( isMC ){
      gSystem->Load("StEEmcSimulatorMaker");
      // other things
      cerr << "NOT YET PROGRAMMED" << endl;
      return;
   };

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
   muDstMaker = new StMuDstMaker( 0, 0, "", inputFileName, "MuDst", nfiles );
   muDstMaker->SetStatus("*",0);
   muDstMaker->SetStatus("Event",1);
   muDstMaker->SetStatus("MuEvent",1);
   muDstMaker->SetStatus("PrimaryVertices",1);
   muDstMaker->SetStatus("EmcAll",1);

   //
   // Connect to the STAR databse
   //
   starDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");
  
   if( isMC ){  
      //
      // Setup ideal gains for processing MC data
      //
      starDatabase->SetFlavor("sim","eemcPMTcal");
      starDatabase->SetFlavor("sim","eemcPIXcal");
      starDatabase->SetFlavor("sim","eemcPMTped");
      starDatabase->SetFlavor("sim","eemcPMTstat");
      starDatabase->SetFlavor("sim","eemcPMTname");
      starDatabase->SetFlavor("sim","eemcADCconf");
      starDatabase->SetDateTime(20050101,0);
   }

   //
   // Initialize EEMC database
   //
   eemcDbMaker = new StEEmcDbMaker("eemcDb");

   // this is the interface to the STAR logger
   // SwitchOn("D") turns on debugging
   // SwitchOff("D") turns off debuggin
   // "I" is info and "W" is warn
   gMessMgr->SwitchOff("D");
   gMessMgr->SwitchOff("I");
   gMessMgr->SetLevel(2);

   //
   // create spin DB maker
   //
   spinDb = new StSpinDbMaker("spinDb");

   StEEmcSlowMaker *slowSim = 0;

   if( isMC ){
      //
      // Initialize slow simulator
      //
      slowSim = new StEEmcSlowMaker("slowSim");
      slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
      slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
      slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
      slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values
   }

   //
   // Energy to ADC maker
   //
   a2EMakerPtr = new StEEmcA2EMaker("EEmcA2EMaker");
   a2EMakerPtr->database("eemcDb");          // sets db connection
   a2EMakerPtr->source("MuDst",1);           // sets mudst as input
   a2EMakerPtr->threshold(3.0,0);            // tower threshold
   a2EMakerPtr->threshold(3.0,1);            // pre1 threshold 
   a2EMakerPtr->threshold(3.0,2);            // pre2 threshold
   a2EMakerPtr->threshold(3.0,3);            // post threshold
   a2EMakerPtr->threshold(3.0,4);            // smdu threshold
   a2EMakerPtr->threshold(3.0,5);            // smdv threshold

   //
   // Now start things particular to the StEEmcTreeMaker
   //

   // Spin info maker
   if( doSpinIO )
      spinInfoMakerPtr = new StSpinInfoMaker_t( "SpinInfoMaker" );

   // Energy Structure Maker
   energyMakerPtr = new StEEmcEnergyMaker_t( "energyMkr", "EEmcA2EMaker" );
   energyMakerPtr->setStripThres( 0.0005 );
   energyMakerPtr->setTowerThres( 0.5 );

   // Tree Maker
   treeWriterPtr = new StEEmcTreeMaker_t( "EEmcTreeMkr" );
   treeWriterPtr->setTreeStatus( StEEmcTreeMaker_t::PART_1, StEEmcTreeMaker_t::WRITE,  outputFileName );
   treeWriterPtr->setTreeStatus( StEEmcTreeMaker_t::PART_2, StEEmcTreeMaker_t::IGNORE, "" );
   treeWriterPtr->setTreeStatus( StEEmcTreeMaker_t::PART_3, StEEmcTreeMaker_t::IGNORE, "" );
   treeWriterPtr->setMaxNumEvents( neventsIn );
   treeWriterPtr->setEEmcEnergyMkr( energyMakerPtr );
   treeWriterPtr->doSpinInfoIO( 1 );
   treeWriterPtr->doEvtHddrIO( 1 );
   treeWriterPtr->addTrigger( trigID );
   treeWriterPtr->setMinNumTowers( 0 );
   treeWriterPtr->setMinNumStrips( 0 );

   if( doSpinIO )
      treeWriterPtr->setSpinInfoMkr( spinInfoMakerPtr );

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

   // If neventsIn/Out is negative, reset to a large value
   // for an Int_t
   if( neventsIn < 0 )
      neventsIn = 1<<30-1;
   if( neventsOut < 0 )
      neventsOut = 1<<30-1;

   Int_t ierr  = kStOK;  // err flag
   Long_t nevents = 0;    // cumulative number of events in
   for( ; nevents < neventsIn && treeWriterPtr->getNumPart1EventsWritten() < neventsOut && !ierr; ++nevents ){
      // clear
      analysisChain->Clear();

      // make
      ierr = analysisChain->Make();

      // Print every so many events
      if( !((nevents+1) % displayFreq) )
         std::cout << "***** finished event number " << nevents << ", " << treeWriterPtr->getNumPart1EventsWritten() << " *****" << std::endl;

      if( ierr ){
         std::cout << "***** ERROR FLAG " << ierr << " on event number " << nevents << " *****" << endl;
      };
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

void loadEEmcTreeLibs(){
   // commong shared libraries
   gROOT->Macro("loadMuDst.C");

   // and a few others
   gSystem->Load("StSpinDbMaker");
   gSystem->Load("StEEmcUtil");

   gSystem->Load("libSpectrum");
   gSystem->Load("libMinuit");

   gSystem->Load("StEEmcPoolEEmcTreeContainers");
   gSystem->Load("StEEmcHitMaker");
   gSystem->Load("StEEmcPointMap");
   gSystem->Load("StEEmcTreeMaker");
};

/*
 * $Id: makeEEmcTreePart2and3_TSIU.C,v 1.1 2012/12/17 20:01:38 sgliske Exp $
 * $Log: makeEEmcTreePart2and3_TSIU.C,v $
 * Revision 1.1  2012/12/17 20:01:38  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker/macros
 *
 *
 */
