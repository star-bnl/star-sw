/***************************************************************************
 *
 * $Id: fgtRawSpectraQA.C,v 1.3 2012/03/05 20:34:48 sgliske Exp $
 * Author: S. Gliske, Jan 2011
 *
 ***************************************************************************
 *
 * Description: Make plot of raw spectra per quadrant.  Requires the DB.
 *
 ***************************************************************************
 *
 * $Log: fgtRawSpectraQA.C,v $
 * Revision 1.3  2012/03/05 20:34:48  sgliske
 * update--still not really working
 *
 * Revision 1.2  2012/01/31 12:53:28  sgliske
 * updates
 *
 * Revision 1.1  2012/01/31 09:26:18  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.3  2012/01/26 11:40:21  sgliske
 * default to cutting short events
 *
 * Revision 1.2  2012/01/25 12:06:03  sgliske
 * default number of events to read changed to -1
 *
 * Revision 1.1  2012/01/24 11:55:39  sgliske
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
class StFgtQaRawOctAdc;


StChain           *analysisChain = 0;
St_db_Maker       *dbMkr         = 0;
StFgtDbMaker      *fgtDbMkr      = 0; 
StFgtRawDaqReader *daqRdr        = 0;
StFgtQaRawOctAdc  *qaMkr         = 0;

TCanvas *can = 0;

int fgtRawSpectraQA( const Char_t *filenameIn = "testfile.daq",
                     const Char_t *filebaseOut = "testfile.rawQA",
                     Int_t date = 20120115,
                     Int_t time = 0,
                     Float_t pedRelThres = 0,    // 0 = no cut, otherwise: # of sigma above pedestal for cut
                     Int_t nevents = 2000,
                     Int_t timebin = 2,
                     UInt_t statusMask,
                     Bool_t cutShortEvents = 1 ){

   LoadLibs();
   Int_t ierr = 0;

   //
   // START CONSTRUCTING THE CHAIN
   //

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   //
   // THE DATABASE
   //

   // note: DB is used to convert elec coords into geoIds in
   // StEvent/StFgtStrip, and do determine the disc and quad given
   // an rdo/arm/apv combination.

   cout << "Constructing St_db_Maker" << endl;

   TString dir0 = "MySQL:StarDb";
   TString dir1 = "$STAR/StarDb";
   St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );
   dbMkr->SetDateTime(date,time);

   cout << "Constructing StFgtDbMaker" << endl;
   fgtDbMkr = new StFgtDbMaker( "fgtDbMkr" );

   //
   // NOW THE OTHER READERS AND MAKERS
   //

   cout << "Constructing the daq reader" << endl;
   daqRdr = new StFgtRawDaqReader( "daqReader", filenameIn, "" );
   daqRdr->setIsCosmic( 0 );
   daqRdr->cutShortEvents( cutShortEvents );

//    a2cMkr = new StFgtA2CMaker( "a2cMkr" );
//    a2cMkr->setStatusMask( statusMask );
//    a2cMkr->setAbsThres( -10000 );  // set to below -4096 to skip cut
//    a2cMkr->setRelThres( pedRelThres );  // set to zero to skip cut

   cout << "Constructing the QA Makers" << endl;
   qaMkr = new StFgtQaRawOctAdc( "qaMkr", 2 );
   qaMkr->setTimeBin( 2 );

   // debug
   analysisChain->ls(4);

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

   cout << "Making plots" << endl;

   gROOT->SetStyle("Plain");
   gStyle->SetOptStat(0);
   gStyle->SetPalette(1);

   can = new TCanvas( "fgtRawQA", "fgtRawQA", 850, 1100);
   can->Divide( 1, 4 );

   // open output file
   can->Print( (std::string(filebaseOut) + ".ps[").data() );

   //std::vector< TH2F* >& hist = qaMkr->getHistVec();

   cout << "First hist at " << qaMkr->getHist(0) << endl;

   cout << "Finding max" << endl;

   // get max
   Float_t max = 0;
   for( Int_t idx = 0; idx < 48; ++idx ){
      if( qaMkr->getHist(idx) )
         if( qaMkr->getHist(idx)->GetMaximum() > max )
            max = qaMkr->getHist(idx)->GetMaximum();
   };
   max = 0.9*max;

   cout << "Max is " << max << endl;

   TH2F *dummy[4];
   const Char_t dummyNames[4][20] = { "dummy1", "dummy2", "dummy3", "dummy4" };

   Float_t xMin = qaMkr->getHist(0)->GetXaxis()->GetXmin();
   Float_t xMax = qaMkr->getHist(0)->GetXaxis()->GetXmax();
   Float_t yMin = qaMkr->getHist(0)->GetYaxis()->GetXmin();
   Float_t yMax = qaMkr->getHist(0)->GetYaxis()->GetXmax();

   for( Int_t i=0; i<4; ++i )
      dummy[i] = new TH2F( dummyNames[i], "", 5, xMin, xMax, 1, yMin, yMax );

   std::stringstream ss;

   Int_t subpad = 1;
   idx = 0;
   for( Int_t rdo = 1; rdo < 3; ++rdo ){
      for( Int_t arm = 0; arm < 6; ++arm ){
         for( Int_t startIdx = 0; startIdx < 4; ++startIdx, ++idx ){
            for( Int_t oct = 0; oct < 1; ++oct ){

               cout << "rdo/arm/disc/oct = " << rdo << '/' << arm << '/' << startIdx << '/' << oct << endl;
               if( qaMkr->getHist(idx)->GetEntries() > 0 ){
                  can->cd(subpad);
                  gPad->SetLeftMargin( 0.06 );
                  gPad->SetRightMargin( 0.05 );
                  gPad->SetBottomMargin( 0.11 );

                  qaMkr->getHist(idx)->SetMaximum( max );
                  dummy[subpad-1]->GetXaxis()->SetTitleOffset(0.9);
                  dummy[subpad-1]->GetXaxis()->SetTitleSize(0.06);
                  dummy[subpad-1]->GetXaxis()->SetLabelSize(0.06);
                  dummy[subpad-1]->GetYaxis()->SetTitleOffset(0.55);
                  dummy[subpad-1]->GetYaxis()->SetTitleSize(0.06);
                  dummy[subpad-1]->GetYaxis()->SetLabelSize(0.06);

                  gPad->SetGridx(0);
                  gPad->SetGridy(0);

                  for( Int_t i = 0; i<5; ++i ){
                     ss.str("");
                     ss.clear();
                     ss << "channels in APV "; // startArray[startIdx]+i;
                     dummy[subpad-1]->GetXaxis()->SetBinLabel( i+1, ss.str().data() );
                  };
                  dummy[subpad-1]->GetXaxis()->SetNdivisions(222,0);
                  dummy[subpad-1]->SetMinimum( 0 );
                  dummy[subpad-1]->SetMaximum( max );
                  dummy[subpad-1]->SetTitle( qaMkr->getHist(idx)->GetTitle() );
                  qaMkr->getHist(idx)->SetTitle("");

                  dummy[subpad-1]->GetYaxis()->SetTitle( qaMkr->getHist(idx)->GetYaxis()->GetTitle() );
                  qaMkr->getHist(idx)->GetYaxis()->SetTitle("");
                  dummy[subpad-1]->Draw("COLZ");

                  qaMkr->getHist(idx)->Draw("COLZ SAME");

                  gPad->Update();
                  gPad->Modified();

                  TPaveText *title = (TPaveText*)(gPad->GetPrimitive("title"));
                  if( title ){
                     title->SetX1NDC( 0.045 );
                     title->SetX2NDC( 0.55 );
                     title->SetY1NDC( 0.91 ) ;
                     title->SetY2NDC( 0.999 );
                     title->SetBorderSize(0);
                     title->SetTextAlign( 12 );
                     title->SetTextColor(kBlue);
                     title->Draw();
                  };

                  TPave *palette = (TPave*)(gPad->GetPrimitive("palette"));
                  if( palette ){
                     palette->SetX1NDC( 0.955 );
                     palette->SetX2NDC( 0.985 );
                     palette->Draw();
                  };

                  ++subpad;
                  if( subpad == 5 ){
                     subpad = 1;
                     can->Print( (std::string(filebaseOut) + ".ps").data() );
                  };
               };
            };
         };
      };
   };

   if( subpad != 1 )
      can->Print( (std::string(filebaseOut) + ".ps").data() );
   can->Print( (std::string(filebaseOut) + ".ps]").data() );

   gSystem->Exec(( std::string("ps2pdf -dAutoRotatePages=/None ") + filebaseOut + ".ps" ).data());


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

  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbBroker");
  cout << "loaded DB libraries" << endl;

  gSystem->Load("RTS");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtDbMaker");
  gSystem->Load("StFgtA2CMaker");
  gSystem->Load("StFgtRawDaqReader");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtQaMakers");
  cout << "loaded FGT libraries" << endl;
};
