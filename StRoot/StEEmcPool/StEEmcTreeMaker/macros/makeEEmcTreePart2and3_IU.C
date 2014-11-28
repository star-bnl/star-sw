/* 
 * Created May 2012, by S. Gliske
 *
 * Macro to read in "Part 1" of the EEmcTree and write out "Part 2"
 * and "Part 3" using the IU algorithm.
 * 
 */

// forward declarations
class StChain;

// algos
class StEEmcHitMakerSimple_t;
class StEEmcTowerClusterFinder_t;
//class StEEmcTowerClusterFinderMinesweeper_t;
class StEEmcStripClusterFinder_t;
class StEEmcStripClusterFinderIU_t;
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
StEEmcStripClusterFinderIU_t            *stripClusterFinderPtr = 0;
StEEmcPointFinderIU_t                   *pointFinderPtr        = 0;
StEEmcEnergyApportionerIU_t             *energyApportionerPtr  = 0;
StEEmcHitMakerSimple_t                  *hitMakerPtr           = 0;
StEEmcTreeMaker_t                       *treeReaderPtr         = 0;
StEEmcTreeMaker_t                       *treeWriterPtr         = 0;

//
// the main routine
//
void makeEEmcTreePart2and3_IU( const Char_t *eemcTreePart1FileName = "eemcTreeP1.root",
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
   stripClusterFinderPtr = new StEEmcStripClusterFinderIU_t();
   stripClusterFinderPtr->setSeedFloorConst(1);

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
   Long_t nIn = -1;
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
 * $Id: makeEEmcTreePart2and3_IU.C,v 1.1 2012/12/17 20:01:31 sgliske Exp $
 * $Log: makeEEmcTreePart2and3_IU.C,v $
 * Revision 1.1  2012/12/17 20:01:31  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker/macros
 *
 *
 */
