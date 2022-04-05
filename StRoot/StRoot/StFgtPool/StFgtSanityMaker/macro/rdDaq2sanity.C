/***************************************************************************
 *
 * Author: Jan Balewski , February 2012
 *
 ***************************************************************************
 *
 * Description: detects & counts bad APVs in FGT events
 *
 ***************************************************************************/


int rdDaq2sanity( const Char_t *filenameIn = "st_physics_13027045_raw_0010001.daq",
               Int_t nevents = 1000 ){
  LoadLibs();
  Int_t ierr = 0;
  
  std::string daqFileName( filenameIn );

  // create histogram storage array  (everybody needs it):
  TObjArray* HList=new TObjArray;

  cout << "Constructing the chain" << endl;
  analysisChain = new StChain("eemcAnalysisChain");
  
  std::string fgtDbMkrName = "";
  
  cout << "Loading St_db_Maker" << endl;
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbBroker");
  
  TString dir0 = "MySQL:StarDb";
  TString dir1 = "$STAR/StarDb";
  St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );
  dbMkr->SetDateTime(20120207,0);      // Feb 7
  //dbMkr->SetDateTime(20120125,80350);      // run 13025001 2012-01-25 08:03:34 GMT
  cout << "Loading StFgtDbMaker" << endl;
  gSystem->Load("StFgtDbMaker");
  
  cout << "Constructing StFgtDbMaker" << endl;
  fgtDbMkr = new StFgtDbMaker( "fgtDbMkr" );
  
  fgtDbMkrName = fgtDbMkr->GetName();
  
  cout << "Fgt DB Maker Name " << fgtDbMkrName << endl;
 
  cout << "Constructing the daq reader" << endl;
  daqRdr = new StFgtRawDaqReader( "daqReader", filenameIn, fgtDbMkrName.data() );
  daqRdr->setIsCosmic( 0 );
  daqRdr->cutShortEvents( true );
  cout << "Fgt DB Maker Name " << fgtDbMkrName.data() << endl;
  cout << "Constructing the Time Shape Maker" << endl;
  // new StFgtSanityMaker( "FgtTimeShapeMaker", fgtDbMkrName.data() );
  gainMk=new StFgtJanGainMaker(  fgtDbMkrName.data() );
  gainMk->setHList(HList);

   // debug
   analysisChain->ls(3);

   cout << "Initializing" << endl;
   ierr = analysisChain->Init();

   if( nevents < 0 )nevents = 1<<30; // a big number

   cout << "max nevents = " << nevents << endl;
   for( int i=0; i<nevents && !ierr; ++i ){
     //    if( i % 100 == 0 )
         cout << "\ton event number **************" << i << endl;

      analysisChain->Clear();
      ierr = analysisChain->Make();
   };

   cout << "M: finish" << endl;
   analysisChain->Finish();

   cerr << "\tall done" << endl;

   TString outFh=filenameIn; outFh="pulse.hist.root";
   //outFh.ReplaceAll(".daq",".wana.hist.root");
   cout<<"Output histo file "<<outFh<<endl;
   hf=new TFile(outFh,"recreate");
   if(hf->IsOpen()) {
     HList->ls();
     HList->Write();
     printf("\n Histo saved -->%s<\n",outFh.Data());
   } else {
     printf("\n Failed to open Histo-file -->%s<, continue\n",outFh.Data());
   }


   fgtDbMkr->printFgtDumpCSV("fgtMapDump.csv"); // to dump whole FGT DB mapping

 
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
  gSystem->Load("StFgtSanityMaker");
  gSystem->Load("StFgtDbMaker");
};

/**************************************************************************
 *
 * $Log: rdDaq2sanity.C,v $
 * Revision 1.4  2012/02/07 08:25:32  balewski
 * *** empty log message ***
 *
 * Revision 1.3  2012/02/07 05:33:33  balewski
 * *** empty log message ***
 *
 * Revision 1.2  2012/02/06 04:17:43  balewski
 * added 2012 APV exclusions
 *
 * Revision 1.1  2012/02/04 22:03:43  balewski
 * start
 *
 *
 **************************************************************************/
