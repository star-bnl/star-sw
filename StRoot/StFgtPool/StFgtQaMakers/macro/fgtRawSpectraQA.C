/***************************************************************************
 *
 * $Id: fgtRawSpectraQA.C,v 1.1 2012/01/31 09:26:18 sgliske Exp $
 * Author: S. Gliske, Jan 2011
 *
 ***************************************************************************
 *
 * Description: Make plot of raw spectra per quadrant.
 *
 ***************************************************************************
 *
 * $Log: fgtRawSpectraQA.C,v $
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
StFgtQaRawOctAdc  *qaMkr[48] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

TCanvas *can = 0;

int fgtRawSpectraQA( const Char_t *filenameIn = "testfile.daq",
                     const Char_t *filebaseOut = "testfile.rawQA",
                     Int_t nevents = -1,
                     Int_t timebin = 2,
                     Int_t isCosmic = -1,
                     Bool_t cutShortEvents = 1 ){

   LoadLibs();
   Int_t ierr = 0;

   // 
   // DECIDE IF COSMIC OR NOT
   // 

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

   //
   // START CONSTRUCTING THE CHAIN
   //

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   std::string fgtDbMkrName = "";

   if( !isCosmic ){
      // always cut short events if it is cosmic data
      cutShortEvents = 1;

      // note: DB is used to convert elec coords into geoIds in
      // StEvent/StFgtStrip, but geoIds are never used in the makers
      // in this macro.

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

   cout << "Constructing the QA Makers" << endl;
   Int_t idx = 0;
   std::stringstream ss;
   Int_t startArray[] = {0,5,12,17};
   for( Int_t rdo = 1; rdo < 3; ++rdo ){
      for( Int_t arm = 0; arm < 6; ++arm ){
         for( Int_t startIdx = 0; startIdx < 4; ++startIdx, ++idx ){
            for( Int_t oct = 0; oct < 1; ++oct ){
               ss.str("");
               ss.clear();
               ss << "fgtRawQaMkr_" << idx;

               qaMkr[idx] = new StFgtQaRawOctAdc( ss.str().data(), rdo, arm, startArray[startIdx], 2 );
            };
         };
      };
   };

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

   cout << "Making plots" << endl;

   gROOT->SetStyle("Plain");
   gStyle->SetOptStat(0);
   gStyle->SetPalette(1);

   can = new TCanvas( "fgtRawQA", "fgtRawQA", 850, 1100);
   can->Divide( 1, 4 );

   // open output file
   can->Print( (std::string(filebaseOut) + ".ps[").data() );

   // get max
   Float_t max = 0;
   for( Int_t idx = 0; idx < 48; ++idx ){
      TH2F *hist = qaMkr[idx]->getHist();
      if( hist->GetMaximum() > max )
         max = hist->GetMaximum();
   };
   max = 0.9*max;

   TH2F *dummy[4];
   const Char_t dummyNames[4][20] = { "dummy1", "dummy2", "dummy3", "dummy4" };

   Float_t xMin = qaMkr[0]->getHist()->GetXaxis()->GetXmin();
   Float_t xMax = qaMkr[0]->getHist()->GetXaxis()->GetXmax();
   Float_t yMin = qaMkr[0]->getHist()->GetYaxis()->GetXmin();
   Float_t yMax = qaMkr[0]->getHist()->GetYaxis()->GetXmax();

   for( Int_t i=0; i<4; ++i )
      dummy[i] = new TH2F( dummyNames[i], "", 5, xMin, xMax, 1, yMin, yMax );

   Int_t subpad = 1;
   idx = 0;
   for( Int_t rdo = 1; rdo < 3; ++rdo ){
      for( Int_t arm = 0; arm < 6; ++arm ){
         for( Int_t startIdx = 0; startIdx < 4; ++startIdx, ++idx ){
            for( Int_t oct = 0; oct < 1; ++oct ){

               TH2F *hist = qaMkr[idx]->getHist();
               if( hist->GetEntries() > 0 ){
                  can->cd(subpad);
                  gPad->SetLeftMargin( 0.06 );
                  gPad->SetRightMargin( 0.05 );
                  gPad->SetBottomMargin( 0.11 );

                  hist->SetMaximum( max );
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
                     ss << "channels in APV " << startArray[startIdx]+i;
                     dummy[subpad-1]->GetXaxis()->SetBinLabel( i+1, ss.str().data() );
                  };
                  dummy[subpad-1]->GetXaxis()->SetNdivisions(222,0);
                  dummy[subpad-1]->SetMinimum( 0 );
                  dummy[subpad-1]->SetMaximum( max );
                  dummy[subpad-1]->SetTitle( hist->GetTitle() );
                  hist->SetTitle("");

                  dummy[subpad-1]->GetYaxis()->SetTitle( hist->GetYaxis()->GetTitle() );
                  hist->GetYaxis()->SetTitle("");
                  dummy[subpad-1]->Draw("COLZ");

                  hist->Draw("COLZ SAME");

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

  gSystem->Load("RTS");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtStatusMaker");
  gSystem->Load("StFgtA2CMaker");
  gSystem->Load("StFgtQaMakers");
};
