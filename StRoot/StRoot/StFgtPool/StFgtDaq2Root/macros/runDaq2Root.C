/***************************************************************************
 *
 * $Id: runDaq2Root.C,v 1.2 2012/06/20 20:21:35 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: runs StFgtDaq2Root on cosmic data.
 *
 ***************************************************************************
 *
 * $Log: runDaq2Root.C,v $
 * Revision 1.2  2012/06/20 20:21:35  sgliske
 * update
 *
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
                 Int_t nevents = -1,
                 Int_t date = 20120125,
                 Int_t time = 80350       // run 13025001 2012-01-25 08:03:34 GMT
                 ){

   LoadLibs();
   Int_t ierr = 0;

   cerr << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   //
   // Set up the date base
   //
   TString dir0 = "MySQL:StarDb";
   TString dir1 = "$STAR/StarDb";
   St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );

   dbMkr->SetDateTime(date,time);

   cout << "Loading StFgtDbMaker" << endl;
   gSystem->Load("StFgtDbMaker");

   cout << "Constructing StFgtDbMaker" << endl;
   fgtDbMkr = new StFgtDbMaker( "fgtDbMkr" );

   //
   // Raw DAQ reader
   //
   cout << "Constructing the daq reader" << endl;
   daqRdr = new StFgtRawDaqReader( "daqReader", filenameIn, "fgtDbMkr" );
   daqRdr->setIsCosmic( 0 );
   daqRdr->cutShortEvents( 1 );

   cerr << "Constructing the root makers" << endl;
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
   // commong shared libraries
   gROOT->Macro("loadMuDst.C");
   //gROOT->Macro("LoadLogger.C");

   // and a few others
   gSystem->Load("libStDb_Tables.so");
   gSystem->Load("StDbLib");
   gSystem->Load("StDbBroker");
   gSystem->Load("St_db_Maker");
   gSystem->Load("StStarLogger");
   gSystem->Load("StFgtDbMaker");

   gSystem->Load("RTS");

   gSystem->Load("StFgtUtil");
   gSystem->Load("StFgtRawDaqReader");
   gSystem->Load("StFgtDaq2Root");
};

