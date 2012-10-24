
// forward declarations
class StChain;
class StMuDstMaker;    
class StMuFgtOccTxtMkr;
class StFgtGenPlotter;
class StFgtGenAVEMaker;
class StFgtDbMaker;

// global variables
StChain            *analysisChain = 0;
StMuDstMaker       *muDstMaker    = 0;
StFgtGenPlotter    *fgtGenPlotter     = 0;
StFgtGenAVEMaker    *fgtAVEffMkr     = 0;
StFgtDbMaker *fgtDbMkr=0;

void runMuDstAVEff( const Char_t *filename, const Char_t* baseFilename=".",
                             Int_t neventsIn = 200 ){
  cout <<" looking at file: " << filename << endl;
   // load the shared libraries
   std::cout << "***** Loading libraries *****" << endl;
   LoadLibs();

   std::cout << "***** Done loading libraries *****" << endl;
   std::cout << "***** Instanciating all the classes *****" << endl;

   //
   // Create the analysis chain
   //
   analysisChain = new StChain("fgtTrackingChain");
   cout << "Loading St_db_Maker" << endl;
   gSystem->Load("libStDb_Tables.so");
   gSystem->Load("StDbLib.so");
   gSystem->Load("St_db_Maker");
   gSystem->Load("StDbBroker");

   TString dir0 = "MySQL:StarDb";
   TString dir1 = "$STAR/StarDb";
   //db maker should not be needed...
   St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );
   dbMkr->SetDateTime(20120310,152618); ///

   //for 13063034
   //     dbMkr->SetDateTime(20120303,130411); ///D
   //      dbMkr->SetDateTime(20120128,204320);      // run ???

   cout << "Constructing StFgtDbMaker" << endl;
   fgtDbMkr = new StFgtDbMaker( "fgtDbMkr" );
   //fgtDbMkr->SetFlavor("ideal",""); // mapping is wrong, but at least the code runs...
   fgtDbMkrName = fgtDbMkr->GetName();


   //
   // MuDst maker for reading input
   //
   Int_t nfiles = 10000;
   muDstMaker = new StMuDstMaker( 0, 0, "", filename, "MuDst", nfiles );
   muDstMaker->SetStatus("*",0);
   muDstMaker->SetStatus("Event",1);
   muDstMaker->SetStatus("MuEvent",1);
   muDstMaker->SetStatus("PrimaryVertices",1);
   muDstMaker->SetStatus("FgtStrip",1);
   muDstMaker->SetStatus("FgtCluster",1);
   muDstMaker->SetStatus("FgtStripAssociation",1);

   //
   // now the QA maker
   //
   // occTxtMkr = new StMuFgtOccTxtMkr( "fgtOccTxtMkr" );

   //
   // the track maker
   //
   //   fgtTrkMkr = new StFgtHHTracking( "fgtTrkMkr" );
   cout <<"construction genplotter " <<endl;
   //fgtGenPlotter = new StFgtGenPlotter( "fgtGenPlotter" );
   fgtAVEffMkr = new StFgtGenAVEMaker( "avEffMkr" );
   fgtAVEffMkr->SetFileBase(baseFilename);

   // debugging info
   std::cout << "***** Done instanciating all the classes *****" << endl;
   analysisChain->ls(3);

   //
   // Initialize all makers
   //
   std::cout << "***** Initializing all makers in the analysis chain *****" << std::endl;
   analysisChain->Init();
   std::cout << "***** Initialization done *****" << std::endl;

   //
   // Finally ready to loop over the events 
   //

   // If nEvents is negative, reset to the maximum possible value
   // for an Int_t
   if( neventsIn < 0 )
      neventsIn = 1<<31-1;

   Int_t ierr  = kStOK;  // err flag
   Int_t nevents = 0;    // cumulative number of events in
   for( ; nevents < neventsIn && !ierr; ++nevents ){
     cout <<"event: "<< nevents <<endl;
      analysisChain->Clear();
          cout <<"making ... " << endl;
      // make
      ierr = analysisChain->Make();
   };
 
   //---------------------------------------------------------------


   //
   // Calls the ::Finish() method on all makers
   //
   analysisChain->Finish(); 

   //
   // Delete the chain
   //
   analysisChain->Delete();

   return;
};



// load the shared libraries
void LoadLibs() {
   // commong shared libraries
   gROOT->Macro("loadMuDst.C");
   //gROOT->Macro("LoadLogger.C");

   // and a few others
//    gSystem->Load("StDbLib");
//    gSystem->Load("StDbBroker");
//    gSystem->Load("St_db_Maker");
//    gSystem->Load("StStarLogger");
   gSystem->Load("StFgtDbMaker");
//   gSystem->Load("StFgtPedPlotter");
//   gSystem->Load("StFgtPedMaker");
//   gSystem->Load("StFgtQaMakers");
   gSystem->Load("StFgtUtil");
   gSystem->Load("StFgtClusterTools");
   //  gSystem->Load("StMuFgtQa");
   //   gSystem->Load("StFgtTracking");
};

/*
$Id: runMuDstAVEff.C,v 1.4 2012/10/24 14:28:57 avossen Exp $
$Log: runMuDstAVEff.C,v $
Revision 1.4  2012/10/24 14:28:57  avossen
adapted macro to new mDst structure

Revision 1.3  2012/05/30 13:38:35  avossen
*** empty log message ***

Revision 1.2  2012/05/25 00:36:11  avossen
made fit for stevent reading

Revision 1.1  2012/04/16 19:50:04  avossen
added cluster tools macro

 *
 */
