/***************************************************************************
 *
 * $Id: makeCosmicPeds.C,v 1.1 2012/01/31 08:52:51 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Compute peds for cosmic data and write to a file
 *
 ***************************************************************************
 *
 * $Log: makeCosmicPeds.C,v $
 * Revision 1.1  2012/01/31 08:52:51  sgliske
 * StFgtPedMaker moved to StFgtPool
 *
 * Revision 1.6  2011/11/14 15:38:50  ckriley
 * made macro to work with new containers
 *
 * Revision 1.5  2011/10/04 18:45:49  sgliske
 * made cut on short events optional
 *
 * Revision 1.4  2011/09/30 19:08:45  sgliske
 * general update
 *
 * Revision 1.3  2011/09/26 16:55:52  sgliske
 * Continued work on cosmic QA plots
 *
 * Revision 1.2  2011/09/22 21:21:12  sgliske
 * first working version
 *
 * Revision 1.1  2011/09/22 14:10:13  sgliske
 * minor update
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtEvent;
class StFgtPedMaker;

StChain *analysisChain          = 0;
StFgtCosmicMaker *cosmicMkr     = 0;
StFgtPedMaker *pedMkr           = 0;

int makeCosmicPeds( const Char_t *filenameIn = "testfile.sfs",
                    const Char_t *filenameOut = "testfile.Ped.txt",
                    Int_t nevents = 199,
                    Short_t timeBinMask = 0x10,
                    Bool_t cutShortEvents = 1 ){


   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filenameIn );
   cosmicMkr->cutShortEvents( cutShortEvents );

   cout << "Constructing the Ped Maker" << endl;
   pedMkr = new StFgtPedMaker( "FgtPedMaker" );
   pedMkr->setToSaveToDb( 0 );
   pedMkr->setToSaveToFile( filenameOut );
   pedMkr->setTimeBinMask( timeBinMask ); // 0x18 = 11000 = bins 3 & 4

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

      //cout << "event number " << i << endl;

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
};
