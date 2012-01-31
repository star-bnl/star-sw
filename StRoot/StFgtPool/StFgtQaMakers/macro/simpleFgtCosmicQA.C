/***************************************************************************
 *
 * $Id: simpleFgtCosmicQA.C,v 1.1 2012/01/31 09:26:18 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Script to make the basic, raw QA plots for a single
 * quadrant
 *
 ***************************************************************************
 *
 * $Log: simpleFgtCosmicQA.C,v $
 * Revision 1.1  2012/01/31 09:26:18  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.6  2011/10/03 19:39:46  avossen
 * compiling version of simple cluster maker, changed PushBack->pushBack energy->charge in ClusterArray and Cluster
 *
 * Revision 1.5  2011/09/28 17:48:50  sgliske
 * minor updates
 *
 * Revision 1.4  2011/09/27 15:28:17  sgliske
 * added common StFgtQaMaker parent
 *
 * Revision 1.3  2011/09/27 00:49:01  sgliske
 * cosmic QA update
 *
 * Revision 1.2  2011/09/26 16:55:53  sgliske
 * Continued work on cosmic QA plots
 *
 * Revision 1.1  2011/09/24 02:14:10  sgliske
 * updated FGT cosmic QA
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtRawMaker;
class StFgtEvent;
class StFgtQaAdcVsChannel;
class StFgtPedPlotter;

StChain *analysisChain          = 0;
StFgtCosmicMaker *cosmicMkr     = 0;

StFgtQaAdcVsChannel *adcVsChan = 0;
StFgtQaAdcVsChannel *adcVsR = 0;
StFgtQaAdcVsChannel *adcVsPhi = 0;
StFgtQaAdcVsChannel *adcSansPedVsChan = 0;
StFgtQaAdcVsChannel *adcSansPedVsR = 0;
StFgtQaAdcVsChannel *adcSansPedVsPhi = 0;
StFgtPedPlotter *pedPlotterC = 0;
StFgtPedPlotter *pedPlotterR = 0;
StFgtPedPlotter *pedPlotterP = 0;

TCanvas *can = 0;

Int_t simpleFgtCosmicQA( const Char_t *filename = "testfile.sfs",
                         const Char_t *pedfilename = "testfile.Ped.txt",
                         Int_t disc = 0,
                         Int_t quad = 0,
                         const Char_t *runname = "testrun",
                         const Char_t *quadname = "010",
                         Int_t nevents = 1999,
                         Int_t timeBin = 4,
                         Int_t numDiscs = 3 ){
   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the ped plotter" << endl;
   pedPlotterC = new StFgtPedPlotter;
   pedPlotterC->setReadFromFile( pedfilename );
   pedPlotterC->setTimeBinMask( 1<<timeBin );
   pedPlotterC->setPlotVsStrip( 'c' );
   pedPlotterC->setDisc( disc );
   pedPlotterC->setQuad( quad );
   pedPlotterC->setQuadName( quadname );

   pedPlotterR = new StFgtPedPlotter;
   pedPlotterR->setReadFromFile( pedfilename );
   pedPlotterR->setTimeBinMask( 1<<timeBin );
   pedPlotterR->setPlotVsStrip( 'R' );
   pedPlotterR->setDisc( disc );
   pedPlotterR->setQuad( quad );
   pedPlotterR->setQuadName( quadname );

   pedPlotterP = new StFgtPedPlotter;
   pedPlotterP->setReadFromFile( pedfilename );
   pedPlotterP->setTimeBinMask( 1<<timeBin );
   pedPlotterP->setPlotVsStrip( 'P' );
   pedPlotterP->setDisc( disc );
   pedPlotterP->setQuad( quad );
   pedPlotterP->setQuadName( quadname );

   cout << "making the graphs" << endl;
   ierr = pedPlotterR->makePlots();
   ierr &= pedPlotterP->makePlots();
   ierr &= pedPlotterC->makePlots();

   if( ierr ){
      return ierr;
   };

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename );
   cout <<"constructing raw maker" <<endl;
   StFgtRawMaker *rawMkr     = new StFgtRawMaker();
   cout <<"done" <<endl;
   cosmicMkr->setNumDiscs( numDiscs );




   cout << "Constructing the QA makers" << endl;

   Float_t sansPedYmax = 512;
   Float_t sansPedYmin = -512;
   Int_t sansPedYbins = (sansPedYmax-sansPedYmin)/8;

   // so that don't have to rebin later
   Int_t binFactorXc = 8, binFactorXr = 2, binFactorXp = 4, binFactorY = 32;

   adcVsChan = new StFgtQaAdcVsChannel( "fgtQaAdc_1", "cosmicMaker", disc, quad, quadname );
   adcVsChan->setTimeBin( timeBin );
   adcVsChan->setFilenameBase( "" );
   adcVsChan->setToPlotVsStrip( 'c' );
   adcVsChan->setToSubtrPeds( 0 );
   adcVsChan->setBinFactors( binFactorXc, binFactorY );

   adcVsR = new StFgtQaAdcVsChannel( "fgtQaAdc_2", "cosmicMaker", disc, quad, quadname );
   adcVsR->setTimeBin( timeBin );
   adcVsR->setFilenameBase( "" );
   adcVsR->setToPlotVsStrip( 'R' );
   adcVsR->setToSubtrPeds( 0 );
   adcVsR->setBinFactors( binFactorXr, binFactorY );

   adcVsPhi = new StFgtQaAdcVsChannel( "fgtQaAdc_3", "cosmicMaker", disc, quad, quadname );
   adcVsPhi->setTimeBin( timeBin );
   adcVsPhi->setFilenameBase( "" );
   adcVsPhi->setToPlotVsStrip( 'P' );
   adcVsPhi->setToSubtrPeds( 0 );
   adcVsPhi->setBinFactors( binFactorXp, binFactorY );

   adcSansPedVsChan = new StFgtQaAdcVsChannel( "fgtQaAdc_4", "cosmicMaker", disc, quad, quadname );
   adcSansPedVsChan->setTimeBin( timeBin );
   adcSansPedVsChan->setFilenameBase( "" );
   adcSansPedVsChan->setToPlotVsStrip( 'c' );
   adcSansPedVsChan->setToSubtrPeds( 1 );
   adcSansPedVsChan->setPedReaderFile( pedfilename );
   //adcSansPedVsChan->setPedThres( 3.0 );
   adcSansPedVsChan->setYbins( sansPedYbins );
   adcSansPedVsChan->setYrange( sansPedYmin, sansPedYmax );
   adcSansPedVsChan->setBinFactors( binFactorXc, 1 );

   adcSansPedVsR = new StFgtQaAdcVsChannel( "fgtQaAdc_5", "cosmicMaker", disc, quad, quadname );
   adcSansPedVsR->setTimeBin( timeBin );
   adcSansPedVsR->setFilenameBase( "" );
   adcSansPedVsR->setToPlotVsStrip( 'R' );
   adcSansPedVsR->setToSubtrPeds( 1 );
   adcSansPedVsR->setPedReaderFile( pedfilename );
   //adcSansPedVsR->setPedThres( 3.0 );
   adcSansPedVsR->setYbins( sansPedYbins );
   adcSansPedVsR->setYrange( sansPedYmin, sansPedYmax );
   adcSansPedVsR->setBinFactors( binFactorXr, 1 );

   adcSansPedVsPhi = new StFgtQaAdcVsChannel( "fgtQaAdc_6", "cosmicMaker", disc, quad, quadname );
   adcSansPedVsPhi->setTimeBin( timeBin );
   adcSansPedVsPhi->setFilenameBase( "" );
   adcSansPedVsPhi->setToPlotVsStrip( 'P' );
   adcSansPedVsPhi->setToSubtrPeds( 1 );
   adcSansPedVsPhi->setPedReaderFile( pedfilename );
   //adcSansPedVsPhi->setPedThres( 3.0 );
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

      if( cosmicMkr->atEOF() )
         ierr = 127;
   };

   cout << "Making plots..." << endl;

   // now do some plotting
   TH2F* hAdcVsChan        = adcVsChan->getHist();       
   TH2F* hAdcVsR           = adcVsR->getHist();          
   TH2F* hAdcVsPhi         = adcVsPhi->getHist();        
   TH2F* hAdcSansPedVsChan = adcSansPedVsChan->getHist();
   TH2F* hAdcSansPedVsR    = adcSansPedVsR->getHist();   
   TH2F* hAdcSansPedVsPhi  = adcSansPedVsPhi->getHist(); 

//    cout << "Hist ptrs " << hAdcVsChan << ' ' << hAdcVsR << ' ' << hAdcVsPhi << ' ' << hAdcSansPedVsChan << ' '
//         << hAdcSansPedVsR << ' ' << hAdcSansPedVsPhi << endl;

   gROOT->SetStyle("Plain");
   gStyle->SetOptStat(0);
   gStyle->SetEndErrorSize(0);
   gStyle->SetTitleBorderSize(0);
   gStyle->SetTitleTextColor(kRed);

   Float_t maxY = pedPlotterC->getMaxY()*1.05;

   TH2F *histC = new TH2F ("histC", "", 1, 0,   1280, 1, 0, maxY );
   TH2F *histR = new TH2F ("histR", "", 1, 10,    40, 1, 0, maxY );
   TH2F *histP = new TH2F ("histP", "", 1, 0,  1.571, 1, 0, maxY );

   TH2F* hAdcArray[9] = { hAdcVsChan, hAdcVsR, hAdcVsPhi,
                           histC, histR, histP,
                           hAdcSansPedVsChan, hAdcSansPedVsR, hAdcSansPedVsPhi };

   TGraphErrors* grArray[3] = { 0, 0, 0 };
   grArray[0] = pedPlotterC->getGraph( timeBin );
   grArray[1] = pedPlotterR->getGraph( timeBin );
   grArray[2] = pedPlotterP->getGraph( timeBin );

   gROOT->SetStyle("Plain");
   TCanvas *can = new TCanvas( "fgtQAcan", "fgtQAcan", 1800, 1200 );

   gStyle->SetOptStat(0);
   gStyle->SetPalette(1);

   can->Divide(3,3);
   for( Int_t i=0; i<9; ++i ){
      can->cd(i+1);
      gPad->SetRightMargin( 0.02 );
      gPad->SetLeftMargin( 0.13 );

      hAdcArray[i]->GetYaxis()->SetTitleOffset( 1.55 );

      if( i < 3 ){
         hAdcArray[i]->SetMaximum( 200 );
         hAdcArray[i]->Draw("COLZ");
      } else if( i < 6 ) {
         hAdcArray[i]->SetTitle( grArray[i-3]->GetTitle() );
         hAdcArray[i]->Draw("H");
         grArray[i-3]->SetLineColor( kBlue );
         grArray[i-3]->Draw("PE same");
      } else {
         hAdcArray[i]->SetMaximum( 100 );
         hAdcArray[i]->Draw("COLZ");
      };
   };

   cout << "ready to save" << endl;

   std::string fileOut = "simpleFgtCosmicQA.";
   fileOut += runname;
   fileOut += ".fgt";
   fileOut += quadname;
   fileOut += ".eps";

   //cout << "\trunname is " << runname << endl;
   cout << "\tsaving to '" << fileOut << "'" << endl;
   can->Print( fileOut.data() );

   //
   // Calls the ::Finish() method on all makers
   //
   cout << "finish" << endl;
   analysisChain->Finish();   // bug in LOG_ makes this fail

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
  cout << "loaded StEvent library" << endl;

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("RTS");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtQaMakers");
  gSystem->Load("StFgtPedPlotter");
};
