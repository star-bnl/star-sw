/***************************************************************************
 *
 * $Id: clusterChargePerAPV.C,v 1.1 2012/01/31 09:26:17 sgliske Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: Script to make cluster charge per apv histograms.
 *
 ***************************************************************************
 *
 * $Log: clusterChargePerAPV.C,v $
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.1  2011/10/10 17:42:06  sgliske
 * creation
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtCorAdcMaker;
class StFgtMaxClusterAlgo;
class StFgtClusterMaker;
class StFgtQaClusterChargePerAPV;

StChain             *analysisChain = 0;
StFgtCosmicMaker    *cosmicMkr     = 0;
StFgtCorAdcMaker    *adcCorrector  = 0;
StFgtMaxClusterAlgo *clusAlgo      = 0;
StFgtClusterMaker   *clusMkr       = 0;
StFgtQaClusterChargePerAPV *histMkr = 0;

TCanvas             *can = 0;

Int_t clusterChargePerAPV( const Char_t *filename = "testfile.sfs",
                           const Char_t *pedfilename = "testfile.Ped.txt",
                           Int_t disc = 0,
                           Int_t quad = 0,
                           const Char_t *runname = "testrun",
                           const Char_t *quadname = "010",
                           Int_t nevents = -1,
                           Int_t timeBin = 4,
                           Bool_t cutShortEvents = 1 ){
   LoadLibs();
   Int_t ierr = 0;

   // hardcoded options
   Int_t numDiscs = 3;
   Int_t nBins_ChPerAPV = 160;
   Float_t min_ChPerAPV = -512;
   Float_t max_ChPerAPV = 2048;

   // canvas size
   Int_t canWidth = 850, canHeight = 1100;

   Short_t timeBinMask = (1<<timeBin);
   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename );
   cosmicMkr->setNumDiscs( numDiscs );

   cout << "Constructing the corrector" << endl;
   adcCorrector = new StFgtCorAdcMaker( "adcCorrector", "cosmicMaker" );
   adcCorrector->setPedReaderFile( pedfilename );
   adcCorrector->setTimeBinMask( timeBinMask );
   adcCorrector->setAbsThres( -10000 );  // set below -4096 to skip cut
   adcCorrector->setRelThres( 5 );       // set to zero to skip cut
   adcCorrector->doSubtrPeds( 1 );
   adcCorrector->doRemoveOtherTimeBins( 1 );

   cout << "Constructing the cluster maker" << endl;
   clusAlgo = new StFgtMaxClusterAlgo();
   clusMkr  = new StFgtClusterMaker( "cosmicMaker", "clusMkr" );
   clusMkr->setClusterAlgo( clusAlgo );

   cout << "Constructing the histogram maker" << endl;
   histMkr = new StFgtQaClusterChargePerAPV( "histMkr", "cosmicMaker", disc, quad, 0, quadname  );

   cout << "Initializing" << endl;
   ierr = analysisChain->Init();
   cout << "\t done initializing" << endl;

   if( ierr ){
      cout << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1<<30; // a big number

   cout << "Looping over events..." << endl;
   for( int i=0; i<nevents && !ierr; ++i ){

      if( i%1000 == 1 )
         cout << "event number " << i << endl;

      //cout << "clear" << endl;
      analysisChain->Clear();

      //cout << "make" << endl;
      ierr = analysisChain->Make();

   };

   //
   // Calls the ::Finish() method on all makers
   //
   cout << "finish" << endl;
   analysisChain->Finish();

   cout << "Making plots..." << endl;





   cout << "all done" << endl;
   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries

  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StEvent");
  gSystem->Load("StUtilities");
  cout << "loaded StEvent library" << endl;

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("RTS");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtQaMakers");
  gSystem->Load("StFgtPedPlotter");
  gSystem->Load("StFgtCorAdcMaker");
  gSystem->Load("StFgtClusterMaker");
};
