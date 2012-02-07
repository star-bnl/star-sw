/***************************************************************************
 *
 * Author: Len K. Eun, Jan 2012
 *
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: timeShape.C,v $
 * Revision 1.1  2012/02/07 03:51:42  leun
 * *** empty log message ***
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtDbMaker;
class StFgtRawDaqReader;
class StFgtTimeShapeMaker;
class St_db_Maker;
class StDbConfigNode;
class StDbManager;

StChain           *analysisChain = 0;
St_db_Maker       *dbMkr         = 0;
StFgtDbMaker      *fgtDbMkr      = 0; 
StFgtRawDaqReader *daqRdr        = 0;
StFgtTimeShapeMaker     *tshapeMkr        = 0;

int timeShape( const Char_t *filenameIn = "st_physics_13036040_raw_5010001.daq", 
               const Char_t *filenameOut = "run13036040.5",
               Int_t nevents = 100000,
               Int_t isCosmic = -1,
               Bool_t cutShortEvents = 0 ){

   LoadLibs();
   Int_t ierr = 0;

   if( isCosmic == -1 ){
      isCosmic = 0;
      std::string daqFileName( filenameIn );
      std::string::size_type pos = daqFileName.find_last_of(".");
      
      if( pos != std::string::npos && daqFileName.substr( pos ) == ".sfs" )
         isCosmic = 1;
   };

   if( isCosmic )
      cout << "Is Cosmic" << endl;
   else
      cout << "Is not cosmic" << endl;


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
     dbMkr->SetDateTime(20120204,202100);      // run 13025001 2012-01-25 08:03:34 GMT
     
     cout << "Loading StFgtDbMaker" << endl;
     gSystem->Load("StFgtDbMaker");
     
     cout << "Constructing StFgtDbMaker" << endl;
     fgtDbMkr = new StFgtDbMaker( "fgtDbMkr" );
     
     fgtDbMkrName = fgtDbMkr->GetName();
     
     cout << "Fgt DB Maker Name " << fgtDbMkrName << endl;
   };
   cout << "Constructing the daq reader" << endl;
   daqRdr = new StFgtRawDaqReader( "daqReader", filenameIn, fgtDbMkrName.data() );
   daqRdr->setIsCosmic( isCosmic );
   daqRdr->cutShortEvents( cutShortEvents );
   cout << "Fgt DB Maker Name " << fgtDbMkrName.data() << endl;
   cout << "Constructing the Time Shape Maker" << endl;
   tshapeMkr = new StFgtTimeShapeMaker( "FgtTimeShapeMaker", fgtDbMkrName.data() );
   std::string outFileName( filenameOut );
   tshapeMkr->fname=outFileName;
   tshapeMkr->fitThresh=500;
   tshapeMkr->plotThresh=700;
   tshapeMkr->fixTau=false;
   tshapeMkr->Ntimebin=7;
   tshapeMkr->pedSelect=1;//pedSelect = 0: time bin 0 for each event, 1: the lowest adc count for each event, 2: from database, event independent.
   

   // debug
   // analysisChain->ls(4);

   cout << "Initializing" << endl;
   ierr = analysisChain->Init();

   if( ierr ){
      cout << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )nevents = 1<<30; // a big number

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
  gSystem->Load("StFgtRawDaqReader");
  gSystem->Load("RTS");
  gSystem->Load("StFgtTimeShapeMaker");
  gSystem->Load("StFgtDbMaker");
};
