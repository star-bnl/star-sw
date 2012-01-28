/***************************************************************************
 *
 * $Id: runDaq2Root.C,v 1.1 2012/01/28 09:29:26 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: runs StFgtDaq2Root on cosmic data.
 *
 ***************************************************************************
 *
 * $Log: runDaq2Root.C,v $
 * Revision 1.1  2012/01/28 09:29:26  sgliske
 * creation
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtRawDaqReader;
class StFgtDaq2RootMaker;

StChain *analysisChain      = 0;
StFgtRawDaqReader  *daqRdr  = 0;
StFgtDaq2RootMaker *rootMkr = 0;

int runDaq2Root( const Char_t *filenameIn = "testfile.sfs",
              const Char_t *filenameOut = "testfile.root",
              Int_t nevents = -1 ){

   LoadLibs();
   Int_t ierr = 0;

   cerr << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cerr << "Constructing the makers" << endl;
   daqRdr = new StFgtRawDaqReader( "daqRdr", filenameIn );
   rootMkr = new StFgtDaq2RootMaker( "daq2root", filenameOut );

   cerr << "Initializing" << endl;
   ierr = analysisChain->Init();

   if( ierr ){
      cerr << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1<<29; // a big number

   for( int i=0; i<nevents && !ierr; ++i ){
      analysisChain->Clear();
      ierr = analysisChain->Make();
   };

   //
   // Calls the ::Finish() method on all makers
   //
   cerr << "finish" << endl;
   analysisChain->Finish(); 

   cerr << "all done" << endl;
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
  cerr << "loaded StEvent library" << endl;

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("RTS");
  gSystem->Load("StFgtDaq2Root");
};
