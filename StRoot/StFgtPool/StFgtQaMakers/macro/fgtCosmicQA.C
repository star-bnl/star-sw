// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtEvent;
class StFgtQaAdcVsChannel;
class StFgtPedPlotter;
class StFgtCorAdcMaker;
class StFgtQaCorrelationPlotMaker;

StChain             *analysisChain = 0;
StFgtCosmicMaker    *cosmicMkr     = 0;
StFgtCorAdcMaker    *adcCorrector  = 0;
StFgtQaAdcVsChannel *adcVsChan     = 0;
StFgtQaAdcVsChannel *adcSansPedVsChan = 0;
StFgtQaAdcVsChannel *adcSansPedVsR1   = 0;
StFgtQaAdcVsChannel *adcSansPedVsR2   = 0;
StFgtQaAdcVsChannel *adcSansPedVsPhi  = 0;
StFgtPedPlotter     *pedPlotter       = 0;
StFgtQaCorrelationPlotMaker *cPlotterC = 0;
StFgtQaCorrelationPlotMaker *cPlotterR1 = 0;
StFgtQaCorrelationPlotMaker *cPlotterR2 = 0;
StFgtQaCorrelationPlotMaker *cPlotterP = 0;

TCanvas             *can1 = 0;
TCanvas             *can2 = 0;
TCanvas             *can3 = 0;

Int_t fgtCosmicQA( const Char_t *filename = "testfile.sfs",
                   const Char_t *pedfilename = "testfile.Ped.txt",
                   Int_t disc = 0,
                   Int_t quad = 0,
                   const Char_t *runname = "testrun",
                   const Char_t *quadname = "010",
                   Int_t nevents = -1,
                   Int_t timeBin = 4,
                   Bool_t cutShortEvents = 1,
                   Bool_t skipCor = 0 ){
   LoadLibs();
   Int_t ierr = 0;

   // hardcoded options
   Int_t numDiscs = 3;
   Float_t sansPedYmax = 1024;
   Float_t sansPedYmin = -512;
   Int_t sansPedYbins = (sansPedYmax-sansPedYmin)/8;

   // so that don't have to rebin later
   Int_t binFactorXc = 8, binFactorXr = 2, binFactorXp = 4, binFactorY = 32;
   Int_t numChannelsPerCorBin = 1;

   // canvas size
   Int_t canWidth = 850, canHeight = 1100;

   Short_t timeBinMask = (1<<timeBin);

   cout << "Constructing the ped plotter" << endl;
   pedPlotter = new StFgtPedPlotter;
   pedPlotter->setReadFromFile( pedfilename );
   pedPlotter->setTimeBinMask( 1<<timeBin );
   pedPlotter->setPlotVsStrip( 'c' );
   pedPlotter->setDisc( disc );
   pedPlotter->setQuad( quad );
   pedPlotter->setQuadName( quadname );
   pedPlotter->setPlotStDev( 1 );

   cout << "making the ped. st. dev. plots" << endl;
   ierr = pedPlotter->makePlots();
   cout << "\tdone" << endl;

   if( ierr ){
      cerr << "\tError making pedistal st. dev. plot" << endl;
      return;
   }

   const TGraph* gr = pedPlotter->getGraph( timeBin );
   if( !gr ){
      cerr << "\tError making pedistal st. dev. plot" << endl;
      return;
   }

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename );
   cosmicMkr->setNumDiscs( numDiscs );

   cout << "Constructing the QA makers" << endl;

   adcVsChan = new StFgtQaAdcVsChannel( "fgtQaAdc_1", "cosmicMaker", disc, quad, quadname );
   adcVsChan->setTimeBin( timeBin );
   adcVsChan->setFilenameBase( "" );
   adcVsChan->setToPlotVsStrip( 'c' );
   adcVsChan->setToSubtrPeds( 0 );
   adcVsChan->setBinFactors( binFactorXc, binFactorY );

   if( !skipCor ){
      cPlotterC = new StFgtQaCorrelationPlotMaker( "cPlotter", "cosmicMaker", disc, quad, quadname );
      cPlotterC->setTimeBin( timeBin );
      cPlotterC->setToPlotVsStrip( 'c' );
      cPlotterC->setBinFactors( numChannelsPerCorBin, 1 );
      cPlotterC->setComputeCorrelations( 1 );
      cPlotterC->setPedReaderFile( "" );

      cPlotterR1 = new StFgtQaCorrelationPlotMaker( "cPlotterR1", "cosmicMaker", disc, quad, quadname );
      cPlotterR1->setTimeBin( timeBin );
      cPlotterR1->setToPlotVsStrip( 'r' );
      cPlotterR1->setBinFactors( numChannelsPerCorBin, 1 );
      cPlotterR1->setComputeCorrelations( 1 );
      cPlotterR1->setPedReaderFile( "" );

      cPlotterR2 = new StFgtQaCorrelationPlotMaker( "cPlotterR2", "cosmicMaker", disc, quad, quadname );
      cPlotterR2->setTimeBin( timeBin );
      cPlotterR2->setToPlotVsStrip( 'R1' );
      cPlotterR2->setBinFactors( numChannelsPerCorBin, 1 );
      cPlotterR2->setComputeCorrelations( 1 );
      cPlotterR2->setPedReaderFile( "" );

      cPlotterP = new StFgtQaCorrelationPlotMaker( "cPlotterP", "cosmicMaker", disc, quad, quadname );
      cPlotterP->setTimeBin( timeBin );
      cPlotterP->setToPlotVsStrip( 'P' );
      cPlotterP->setBinFactors( numChannelsPerCorBin, 1 );
      cPlotterP->setComputeCorrelations( 1 );
      cPlotterP->setPedReaderFile( "" );
   };

   adcCorrector = new StFgtCorAdcMaker( "adcCorrector", "cosmicMaker" );
   adcCorrector->setPedReaderFile( pedfilename );
   adcCorrector->setTimeBinMask( timeBinMask );
   adcCorrector->setAbsThres( -10000 );  // set below -4096 to skip cut
   adcCorrector->setRelThres( 0 );       // set to zero to skip cut
   adcCorrector->doSubtrPeds( 1 );
   adcCorrector->doRemoveOtherTimeBins( 1 );

   adcSansPedVsChan = new StFgtQaAdcVsChannel( "fgtQaAdc_3", "cosmicMaker", disc, quad, quadname );
   adcSansPedVsChan->setTimeBin( timeBin );
   adcSansPedVsChan->setFilenameBase( "" );
   adcSansPedVsChan->setToPlotVsStrip( 'c' );
   adcSansPedVsChan->setToSubtrPeds( 0 );
   adcSansPedVsChan->setYbins( sansPedYbins );
   adcSansPedVsChan->setYrange( sansPedYmin, sansPedYmax );
   adcSansPedVsChan->setBinFactors( binFactorXc, 1 );

   adcSansPedVsR1 = new StFgtQaAdcVsChannel( "fgtQaAdc_4", "cosmicMaker", disc, quad, quadname );
   adcSansPedVsR1->setTimeBin( timeBin );
   adcSansPedVsR1->setFilenameBase( "" );
   adcSansPedVsR1->setToPlotVsStrip( 'r' );
   adcSansPedVsR1->setToSubtrPeds( 0 );
   adcSansPedVsR1->setYbins( sansPedYbins );
   adcSansPedVsR1->setYrange( sansPedYmin, sansPedYmax );
   adcSansPedVsR1->setBinFactors( binFactorXr, 1 );

   adcSansPedVsR2 = new StFgtQaAdcVsChannel( "fgtQaAdc_5", "cosmicMaker", disc, quad, quadname );
   adcSansPedVsR2->setTimeBin( timeBin );
   adcSansPedVsR2->setFilenameBase( "" );
   adcSansPedVsR2->setToPlotVsStrip( 'R' );
   adcSansPedVsR2->setToSubtrPeds( 0 );
   adcSansPedVsR2->setYbins( sansPedYbins );
   adcSansPedVsR2->setYrange( sansPedYmin, sansPedYmax );
   adcSansPedVsR2->setBinFactors( binFactorXr, 1 );

   adcSansPedVsPhi = new StFgtQaAdcVsChannel( "fgtQaAdc_6", "cosmicMaker", disc, quad, quadname );
   adcSansPedVsPhi->setTimeBin( timeBin );
   adcSansPedVsPhi->setFilenameBase( "" );
   adcSansPedVsPhi->setToPlotVsStrip( 'P' );
   adcSansPedVsPhi->setToSubtrPeds( 0 );
   adcSansPedVsPhi->setYbins( sansPedYbins );
   adcSansPedVsPhi->setYrange( sansPedYmin, sansPedYmax );
   adcSansPedVsPhi->setBinFactors( binFactorXp, 1 );

   cout << "Initializing" << endl;
   ierr = analysisChain->Init();
   cout << "\t done initializing" << endl;

   if( ierr ){
      cout << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1<<30; // a big number

   cout << "Looping over events..." << endl;
   for( int i=0; i<nevents && !ierr; ++i ){

      if( i%1000 == 1 )
         cout << "event number " << i << endl;

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

   cout << "Making plots..." << endl;

   // now do some plotting
   TH2F* hAdcVsChan        = adcVsChan->getHist();       
   TH2F* hAdcSansPedVsChan = adcSansPedVsChan->getHist();
   TH2F* hAdcSansPedVsR1   = adcSansPedVsR1->getHist();   
   TH2F* hAdcSansPedVsR2   = adcSansPedVsR2->getHist();   
   TH2F* hAdcSansPedVsPhi  = adcSansPedVsPhi->getHist(); 

//    cout << "Hist ptrs " << hAdcVsChan << ' ' << hAdcVsR << ' ' << hAdcVsPhi << ' ' << hAdcSansPedVsChan << ' '
//         << hAdcSansPedVsR << ' ' << hAdcSansPedVsPhi << endl;

   gROOT->SetStyle("Plain");
   gStyle->SetOptStat(0);
   gStyle->SetEndErrorSize(0);
   gStyle->SetTitleBorderSize(0);
   gStyle->SetTitleTextColor(kRed);
   gStyle->SetPalette(1);

   //Float_t maxY = pedPlotter->getMaxY()*1.05;
   TH2F *histC = new TH2F ("histC", "", 1, 0,   1280, 1, 0, 200 );

   gROOT->SetStyle("Plain");
   TCanvas *can1 = new TCanvas( "fgtQAcan1", "fgtQAcan1", canWidth, canHeight );

   can1->Divide(1,4);
   gPad->SetGridx(0);

   // top row
   can1->cd(1);
   gPad->SetGridx(0);
   hAdcVsChan->SetMaximum( 200 );
   hAdcVsChan->Draw("COLZ");

   can1->cd(2);
   gPad->SetGridx(0);
   histC->SetTitle( gr->GetTitle() );
   histC->Draw( "H" );
   gr->SetMarkerColor( kBlue );
   gr->SetMarkerSize( 0.4 );
   gr->SetMarkerStyle( 20 );
   gr->Draw("PE same");

   can1->cd(3);
   gPad->SetGridx(0);
   hAdcSansPedVsChan->SetMaximum( 100 );
   hAdcSansPedVsChan->Draw("COLZ");

   can1->cd(4);
   gPad->SetGridx(0);
   TPad *pad = gPad;
   pad->Divide(3,1);

   pad->cd(1);
   gPad->SetGridx(0);
   hAdcSansPedVsR1->SetMaximum( 100 );
   hAdcSansPedVsR1->Draw("COLZ");

   pad->cd(2);
   gPad->SetGridx(0);
   hAdcSansPedVsR2->SetMaximum( 100 );
   hAdcSansPedVsR2->Draw("COLZ");

   pad->cd(3);
   gPad->SetGridx(0);
   hAdcSansPedVsPhi->SetMaximum( 100 );
   hAdcSansPedVsPhi->Draw("COLZ");

//    gPad->SetRightMargin( 0.02 );
//    gPad->SetLeftMargin( 0.13 );
//    hAdcArray[i]->GetYaxis()->SetTitleOffset( 1.55 );

   if( !skipCor ){
      TCanvas *can2 = new TCanvas( "fgtQAcan2", "fgtQAcan2", canWidth, canWidth );
      TH2F* corHist = cPlotterC->getCorrelationHist();

      gPad->SetGridx(0);
      gPad->SetGridy(0);
      if( corHist ){
         corHist->SetMaximum( 1 );
         corHist->SetMinimum( -1 );
         corHist->Draw("COLZ");
      };
   };

   if( !skipCor ){
      TCanvas *can3 = new TCanvas( "fgtQAcan3", "fgtQAcan3", canWidth, canHeight );
      can3->Divide(1,2);

      TH2F* corHistR1 = cPlotterR1->getCorrelationHist();
      TH2F* corHistR2 = cPlotterR2->getCorrelationHist();
      TH2F* corHistP  = cPlotterP->getCorrelationHist();
      can3->cd(1);

      TVirtualPad *pad = gPad;
      pad->Divide(1,2);

      pad->cd(1);
      gPad->SetGridx(0);
      gPad->SetGridy(0);

      if( corHistR1 ){
         corHistR1->SetMaximum( 1 );
         corHistR1->SetMinimum( -1 );
         corHistR1->Draw("COLZ");
      };

      pad->cd(2);
      gPad->SetGridx(0);
      gPad->SetGridy(0);

      if( corHistR2 ){
         corHistR2->SetMaximum( 1 );
         corHistR2->SetMinimum( -1 );
         corHistR2->Draw("COLZ");
      };

      can3->cd(2);
      gPad->SetGridx(0);
      gPad->SetGridy(0);

      if( corHistP ){
         corHistP->SetMaximum( 1 );
         corHistP->SetMinimum( -1 );
         corHistP->Draw("COLZ");
      };
   };


   cout << "ready to save" << endl;

   std::string fileOut1 = "fgtCosmicQA.page-1.";
   std::string fileOut2 = "fgtCosmicQA.page-2.";
   std::string fileOut3 = "fgtCosmicQA.page-3.";

   std::string tail = runname;
   tail += ".fgt";
   tail += quadname;
   tail += ".pdf";

   //cout << "\trunname is " << runname << endl;
   cout << "\tsaving to '" << tail << "'" << endl;
   can1->Print( (fileOut1+tail).data() );
   if( !skipCor ){
      can2->Print( (fileOut2+tail).data() );
      can3->Print( (fileOut3+tail).data() );
   };

   cout << "all done" << endl;
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
  cout << "loaded StEvent library" << endl;

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("RTS");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtQaMakers");
  gSystem->Load("StFgtPedPlotter");
  gSystem->Load("StFgtCorAdcMaker");

};
