/***************************************************************************
 *
 * $Id: fgtSingleEvents.C,v 1.2 2012/07/31 20:33:13 sgliske Exp $
 * Author: S. Gliske, Jan 2011
 *
 ***************************************************************************
 *
 * Description: Make histograms of single events.
 *
 ***************************************************************************
 *
 * $Log: fgtSingleEvents.C,v $
 * Revision 1.2  2012/07/31 20:33:13  sgliske
 * updated to execute without errors with latest other CVS FGT software
 *
 * Revision 1.1  2012/01/31 09:26:18  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.4  2012/01/28 11:25:59  sgliske
 * changed StFgtA2CMaker::setDb to setFgtDb
 *
 * Revision 1.3  2012/01/24 08:11:12  sgliske
 * Bug fixes
 *
 * Revision 1.2  2012/01/24 06:27:35  sgliske
 * debugged a bit more
 *
 * Revision 1.1  2012/01/24 03:32:19  sgliske
 * creation
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtDbMaker;
class StFgtRawDaqReader;
class St_db_Maker;
class StDbConfigNode;
class StDbManager;
class StFgtA2CMaker;
class StFgtSingleEventQA;

StChain           *analysisChain = 0;
St_db_Maker       *dbMkr         = 0;
StFgtDbMaker      *fgtDbMkr      = 0; 
StFgtRawDaqReader *daqRdr        = 0;
StFgtSingleEventQA *qaMkr        = 0;
StFgtA2CMaker     *a2cMkr        = 0;

int fgtSingleEvents( const Char_t *filenameIn = "testfile.daq",
                     const Char_t *filenameOut = "testfile.root",
                     Int_t nevents = 200,
                     Bool_t isCosmic = 0,
                     Bool_t cutShortEvents = 0 ){

   LoadLibs();
   Int_t ierr = 0;

   if( isCosmic )
      cout << "Is Cosmic" << endl;
   else
      cout << "Is not cosmic" << endl;

   //
   // START CONSTRUCTING THE CHAIN
   //

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   std::string fgtDbMkrName = "";

   if( !isCosmic ){
      // always cut short events if it is cosmic data
      cutShortEvents = 1;

      cout << "Loading St_db_Maker" << endl;
      gSystem->Load("libStDb_Tables.so");
      gSystem->Load("StDbLib.so");
      gSystem->Load("St_db_Maker");
      gSystem->Load("StDbBroker");

      TString dir0 = "MySQL:StarDb";
      TString dir1 = "$STAR/StarDb";
      St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );
      dbMkr->SetDateTime(20120115,1);

      cout << "Loading StFgtDbMaker" << endl;
      gSystem->Load("StFgtDbMaker");

      cout << "Constructing StFgtDbMaker" << endl;
      fgtDbMkr = new StFgtDbMaker( "fgtDbMkr" );
      //fgtDbMkr->SetFlavor("ideal",""); // mapping is wrong, but at least the code runs...

      fgtDbMkrName = fgtDbMkr->GetName();
   };

   //
   // NOW THE OTHER READERS AND MAKERS
   //

   cout << "Constructing the daq reader" << endl;
   daqRdr = new StFgtRawDaqReader( "daqReader", filenameIn, fgtDbMkrName.data() );
   daqRdr->setIsCosmic( isCosmic );
   daqRdr->cutShortEvents( cutShortEvents );

   cout << "Constructing the A2C converter" << endl;
   a2cMkr = new StFgtA2CMaker( "a2cMaker" );
   //a2cMkr->setTimeBinMask( 0xFF );
   a2cMkr->setAbsThres( 100 );  // set to below -4096 to skip cut
   a2cMkr->setRelThres( 0 );  // set to zero to skip cut
   if( !isCosmic )
      a2cMkr->setFgtDb( fgtDbMkr->getDbTables() );
   //a2cMkr->doRemoveOtherTimeBins( 0 );
   //a2cMkr->doCutBadStatus( 0 );

   cout << "Constructing the QA Maker" << endl;
   qaMkr = new StFgtSingleEventQA( "fgtSingleEventQA" );
   qaMkr->setFilename( filenameOut );

   // debug
   // analysisChain->ls(4);

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

      if( i+1 % 100 == 0 )
         cout << "\ton event number " << i << endl;

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

  gSystem->Load("RTS");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtDbMaker");
  gSystem->Load("StFgtRawDaqReader");
//   gSystem->Load("StFgtPedMaker");
//   gSystem->Load("StFgtStatusMaker");
  gSystem->Load("StFgtA2CMaker");
  gSystem->Load("StFgtQaMakers");
};
