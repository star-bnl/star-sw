/***************************************************************************
 *
 * $Id: simpleTestStandTest.C,v 1.1 2011/09/20 15:53:09 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Simple test macro to read in test stand data.
 *
 ***************************************************************************
 *
 * $Log: simpleTestStandTest.C,v $
 * Revision 1.1  2011/09/20 15:53:09  sgliske
 * Update so that everything compiles nicely
 * and so that one can execute the macro/simpleTestStandTest.C file
 *
 *
 **************************************************************************/

class StFgtCosmicMaker;
class StEvent;
class StFgtEvent;

int simpleTestStandTest( const Char_t *filename = "testfile.sfs",
                         Int_t nevents = 10,
                         Int_t numDiscs = 3 ){

   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the maker" << endl;
   StFgtCosmicMaker *cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename, numDiscs );

   cout << "Initializing" << endl;
   ierr = cosmicMkr->Init();

   if( ierr ){
      cout << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1e100; // a big number

   for( int i=0; i<nevents && !ierr; ++i ){

      cosmicMkr->Clear();
      ierr = cosmicMkr->Make();

   };
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
};
