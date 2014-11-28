/***************************************************************************
 *
 * $Id: simpleTestStandTest.C,v 1.1 2012/01/31 09:07:43 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Simple test macro to read in test stand data.
 *
 ***************************************************************************
 *
 * $Log: simpleTestStandTest.C,v $
 * Revision 1.1  2012/01/31 09:07:43  sgliske
 * moved StFgtRawDaqReader to StFgtPool/StFgtRawDaqReader
 *
 * Revision 1.4  2011/09/21 17:49:35  sgliske
 * alternate base class with more
 *  functionality and not an StMaker
 *
 * Revision 1.2  2011/09/21 00:39:57  avossen
 * added simple Fgt maker base class
 *
 * Revision 1.1  2011/09/20 15:53:09  sgliske
 * Update so that everything compiles nicely
 * and so that one can execute the macro/simpleTestStandTest.C file
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StEvent;
class StFgtEvent;

StChain *analysisChain      = 0;
StFgtCosmicMaker *cosmicMkr = 0;

int simpleTestStandTest( const Char_t *filename = "testfile.sfs",
                         Int_t nevents = 10,
                         Int_t numDiscs = 6 ){

   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename );
   cosmicMkr->setNumDiscs( numDiscs );

   cout << "Initializing" << endl;
   ierr = analysisChain->Init();

   if( ierr ){
      cout << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1e100; // a big number

   for( int i=0; i<nevents && !ierr; ++i ){

      cout << "event number " << i << endl;

      cout << "clear" << endl;
      analysisChain->Clear();

      cout << "make" << endl;
      ierr = analysisChain->Make();

      // count number of events
      StFgtEvent *fgtEventPtr = cosmicMkr->getFgtEventPtr();
      for( Int_t disc = 0; disc < numDiscs; ++disc ){
         StFgtDisc *discPtr = fgtEventPtr->getDiscPtr( disc );
         if( discPtr )
            cout << "\tDisc " << disc << ", number of raw hits " << discPtr->getNumRawHits() << endl;
      };
   };

   //
   // Calls the ::Finish() method on all makers
   //
   cout << "finish" << endl;
   analysisChain->Finish(); 

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
  cout << "loaded StEvent library" << endl;

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("RTS");
};
