
// forward declarations
class StChain;
class StMuDstMaker;    
class StMuFgtOccTxtMkr;
class StFgtGenPlotter;
class StFgtGenAVEMaker;
class StFgtGeneralBase;
class StFgtStraightTrackMaker;
class StFgtStraightPlotter;
class StFgtDbMaker;

// global variables
StChain            *analysisChain = 0;
StMuDstMaker       *muDstMaker    = 0;
StFgtGenPlotter    *fgtGenPlotter     = 0;
StFgtGenAVEMaker    *fgtAVEffMkr     = 0;
StFgtGeneralBase * fgtGenBase =0;
StFgtStraightTrackMaker* fgtStraightTrackMaker2;
StFgtStraightPlotter* fgtStraightPlotter2;
StFgtStraightTrackMaker* fgtStraightTrackMaker3;
StFgtStraightPlotter* fgtStraightPlotter3;
StFgtStraightTrackMaker* fgtStraightTrackMaker4;
StFgtStraightPlotter* fgtStraightPlotter4;
StFgtDbMaker *fgtDbMkr=0;

void runMuDstAVEff( const Char_t *filename, const Char_t* baseFilename=".",
                             Int_t neventsIn = 400 ){
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
      dbMkr->SetDateTime(20120622,152618); ///

   //for 13063034
   //     dbMkr->SetDateTime(20120303,130411); ///D
   //      dbMkr->SetDateTime(20120128,204320);      // run ???

   cout << "Constructing StFgtDbMaker" << endl;
   fgtDbMkr = new StFgtDbMaker( "fgtDb" );
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
   muDstMaker->SetStatus("FgtAdc",1);

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
   //   fgtAVEffMkr = new StFgtGenAVEMaker( "avEffMkr" );
   fgtGenBase=new StFgtGeneralBase("fgtGenBase");
   //   fgtGenBase->setVertexNumber(1); //<--- this selects second best vertex
   fgtGenBase->useEHTTrig(true);
   fgtStraightTrackMaker2 =new StFgtStraightTrackMaker("fgtStraightTracker2");
   fgtStraightTrackMaker3 =new StFgtStraightTrackMaker("fgtStraightTracker3");
   fgtStraightTrackMaker4 =new StFgtStraightTrackMaker("fgtStraightTracker4");
   fgtStraightTrackMaker2->SetEffDisk(20);
   fgtStraightTrackMaker3->SetEffDisk(20);
   fgtStraightTrackMaker4->SetEffDisk(20);
   fgtStraightTrackMaker2->setMinNumFitPoints(2);
   fgtStraightTrackMaker3->setMinNumFitPoints(3);
   fgtStraightTrackMaker4->setMinNumFitPoints(4);
   fgtStraightTrackMaker2->setMaxClusters(30);
   fgtStraightTrackMaker3->setMaxClusters(30);
   fgtStraightTrackMaker4->setMaxClusters(30);
   fgtStraightPlotter2=new StFgtStraightPlotter("fgtStraightPlotterNew2","fgtStraightTracker2");
   fgtStraightPlotter3=new StFgtStraightPlotter("fgtStraightPlotter3","fgtStraightTracker3");
   fgtStraightPlotter4=new StFgtStraightPlotter("fgtStraightPlotter4","fgtStraightTracker4");
   fgtStraightPlotter2->SetFileBase(".","2PointTrack");
   fgtStraightPlotter3->SetFileBase(".","3PointTrack");
   fgtStraightPlotter4->SetFileBase(".","4PointTrack");

   //   fgtAVEffMkr->SetFileBase(baseFilename);

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
     if(nevents%100)
       cout <<"event: "<< nevents <<endl;
      analysisChain->Clear();
      //          cout <<"making ... " << endl;
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
$Id: runMuDstAVEff.C,v 1.14 2013/05/10 18:20:09 avossen Exp $
$Log: runMuDstAVEff.C,v $
Revision 1.14  2013/05/10 18:20:09  avossen
implemented cuts on dca in straight tracker

Revision 1.13  2013/05/01 16:49:33  avossen
updated code and macros so that several trackers and plotters can run at the same time with different settings such as min points. Added option to use other primary vertices

Revision 1.12  2013/03/02 03:11:24  avossen
*** empty log message ***

Revision 1.11  2013/01/29 21:58:25  avossen
new/old efficiency computation now consistent

Revision 1.10  2013/01/23 22:52:17  avossen
fixed uninitialized bufffer for filename

Revision 1.9  2013/01/23 20:36:41  avossen
less printouts, no crash

Revision 1.8  2013/01/20 02:06:37  avossen
changed defines in attributes

Revision 1.7  2013/01/18 15:47:15  avossen
update for new mdsts

Revision 1.6  2012/12/22 04:13:33  avossen
*** empty log message ***

Revision 1.5  2012/12/22 02:19:15  avossen
some bugfixes

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
