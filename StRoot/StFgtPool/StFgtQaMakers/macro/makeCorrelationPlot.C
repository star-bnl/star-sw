/***************************************************************************
 *
 * $Id: makeCorrelationPlot.C,v 1.1 2012/01/31 09:26:18 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Plots the correlation coefficients and covariance.
 *
 ***************************************************************************
 *
 * $Log: makeCorrelationPlot.C,v $
 * Revision 1.1  2012/01/31 09:26:18  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.2  2011/09/30 19:05:32  sgliske
 * general update
 *
 * Revision 1.1  2011/09/28 17:48:50  sgliske
 * minor updates
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtEvent;
class StFgtQaCorrelationPlotMaker;

StChain *analysisChain          = 0;
StFgtCosmicMaker *cosmicMkr     = 0;
StFgtQaCorrelationPlotMaker *plotterC = 0;
StFgtQaCorrelationPlotMaker *plotterR = 0;
StFgtQaCorrelationPlotMaker *plotterP = 0;

TCanvas *can = 0;

Int_t makeCorrelationPlot( const Char_t *filename = "testfile.sfs",
                           const Char_t *pedfilename = "testfile.Ped.txt",
                           Int_t disc = 2,
                           Int_t quad = 0,
                           const Char_t *runname = "testrun",
                           const Char_t *quadname = "013",
                           Int_t numChannelsPerBin = 8,
                           Int_t nevents = -1,
                           Int_t timeBin = 4,
                           Int_t numDiscs = 3 ){
   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename );
   cosmicMkr->setNumDiscs( numDiscs );

   cout << "Constructing the QA makers" << endl;

   // so that don't have to rebin later
   Int_t binFactorX = 8;

   plotterC = new StFgtQaCorrelationPlotMaker( "plotterC", "cosmicMaker", disc, quad, quadname );
   plotterC->setTimeBin( timeBin );
   plotterC->setToPlotVsStrip( 'c' );
   plotterC->setBinFactors( numChannelsPerBin, 1 );
   plotterC->setComputeCorrelations( 1 );
   plotterC->setPedReaderFile( pedfilename );

   plotterR = new StFgtQaCorrelationPlotMaker( "plotterR", "cosmicMaker", disc, quad, quadname );
   plotterR->setTimeBin( timeBin );
   plotterR->setToPlotVsStrip( 'R' );
   plotterR->setBinFactors( numChannelsPerBin, 1 );
   plotterR->setComputeCorrelations( 1 );
   plotterR->setPedReaderFile( pedfilename );

   plotterP = new StFgtQaCorrelationPlotMaker( "plotterP", "cosmicMaker", disc, quad, quadname );
   plotterP->setTimeBin( timeBin );
   plotterP->setToPlotVsStrip( 'P' );
   plotterP->setBinFactors( numChannelsPerBin, 1 );
   plotterP->setComputeCorrelations( 1 );
   plotterP->setPedReaderFile( pedfilename );

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

      if( cosmicMkr->atEOF() )
         ierr = 127;
   };

   //
   // Calls the ::Finish() method on all makers
   //
   cout << "finish" << endl;
   analysisChain->Finish();   // bug in LOG_ makes this fail

   cout << "Making plots..." << endl;

   // now do some plotting
   TH2F* hPlots[6] = {
      plotterC->getCovarianceHist(),
      plotterR->getCovarianceHist(),
      plotterP->getCovarianceHist(),
      plotterC->getCorrelationHist(),
      plotterR->getCorrelationHist(),
      plotterP->getCorrelationHist()
   };

   gROOT->SetStyle("Plain");
   gStyle->SetOptStat(0);
   gStyle->SetEndErrorSize(0);
   gStyle->SetTitleBorderSize(0);
   gStyle->SetTitleTextColor(kRed);
   gStyle->SetPalette(1);

   TCanvas *can = new TCanvas( "fgtCorCan", "fgtCorCan", 1800, 1200 );

   can->Divide(3,2);
   for( Int_t i=0; i<6; ++i ){
      can->cd(i+1);
      gPad->SetRightMargin( 0.02 );
      gPad->SetLeftMargin( 0.13 );

      cout << "Plot " << i << ' ' << hPlots[i] << endl;
      if( hPlots[i] ){
         hPlots[i]->GetYaxis()->SetTitleOffset( 1.55 );
         //hPlots[i]->SetMaximum( 200 );
         hPlots[i]->Draw("COLZ");
      };
   };

   cout << "ready to save" << endl;

   std::string fileOut = "fgtCosmic-CorPlot.";
   fileOut += runname;
   fileOut += ".fgt";
   fileOut += quadname;
   fileOut += ".eps";

   //cout << "\trunname is " << runname << endl;
   cout << "\tsaving to '" << fileOut << "'" << endl;
   can->Print( fileOut.data() );

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
};
