/***************************************************************************
 *
 * $Id: makeCosmicStatus.C,v 1.1 2012/01/31 08:59:43 sgliske Exp $
 * Author: C. K. Riley, Nov 2011
 *
 ***************************************************************************
 *
 * Description: Compute hot strips (and possibly other stuff) for cosmic data
 * and write to a file
 *
 ***************************************************************************
 *
 * $Log: makeCosmicStatus.C,v $
 * Revision 1.1  2012/01/31 08:59:43  sgliske
 * moved StFgtStatus maker to StFgtPool
 *
 * Revision 1.3  2011/12/07 17:17:55  ckriley
 * minor update
 *
 * Revision 1.2  2011/12/01 00:41:34  ckriley
 * make compatible with db stuff
 *
 * Revision 1.1  2011/11/25 20:22:37  ckriley
 * creation of statusmaker
 *
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtEvent;
class StFgtA2CMaker;
class StFgtStatusMaker;

StChain *analysisChain          = 0;
StFgtCosmicMaker *cosmicMkr     = 0;
StFgtA2CMaker *adcCorrector     = 0;
StFgtStatusMaker *statusMkr        = 0;

int makeCosmicStatus( const Char_t *filenameIn = "testfile.sfs",
                      const Char_t *pedfilename = "testfile.Ped.txt",
                      const Char_t *filenameOut = "testfile.Status.txt",
                      Int_t nevents = 199,
                      Short_t timeBin = 4,
                      Bool_t cutShortEvents = 1 ){


   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filenameIn );
   cosmicMkr->cutShortEvents( cutShortEvents );

   Short_t timeBinMask = (1<<timeBin);

   cout << "Constructing the A2C maker" << endl;
   adcCorrector = new StFgtA2CMaker( "adcCorrector" );
   adcCorrector->setPedReaderFile( pedfilename );
   adcCorrector->setTimeBinMask( timeBinMask );
   adcCorrector->setAbsThres( -10000 );  // set below -4096 to skip cut
   adcCorrector->setRelThres( 5 );       // set to zero to skip cut
   adcCorrector->doRemoveOtherTimeBins( 1 );


   cout << "Constructing the Status Maker" << endl;
   statusMkr = new StFgtStatusMaker( "FgtStatusMaker" );
   statusMkr->setToSaveToDb( 0 );
   statusMkr->setToSaveToFile( filenameOut );

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

      if( i%1000 == 1 )
         cout << "\n\nevent number " << i << endl;

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

   cerr << "\tall done" << endl;
   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries

  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StUtilities");
  gSystem->Load("StEvent");
  cout << "loaded StEvent library" << endl;

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("RTS");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtStatusMaker");
  gSystem->Load("StFgtA2CMaker");
};
