/***************************************************************************
 *
 * $Id: fgtCosmicTrackQA.C,v 1.1 2012/01/31 09:26:17 sgliske Exp $ 
 * Author: ckriley, Oct. 19 2011 
 *
 ***************************************************************************
 *
 * Description: Script to make the basic, raw track QA plots for middle
 * quadrant
 *
 ***************************************************************************
 *
 * $Log: fgtCosmicTrackQA.C,v $
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.12  2011/12/07 17:19:59  ckriley
 * minor update
 *
 * Revision 1.11  2011/11/25 20:20:04  ckriley
 * added statusmaker functionality
 *
 * Revision 1.10  2011/11/17 18:41:57  sgliske
 * Bug fixes: check on palette ptrs and set A2CMaker to remove pedestals
 *
 * Revision 1.9  2011/11/09 21:03:20  ckriley
 * working version with current containers
 *
 * Revision 1.8  2011/10/28 20:17:44  ckriley
 * conforming with other maker methods of getting event
 *
 * Revision 1.7  2011/10/26 20:59:14  ckriley
 * now correctly subtracts pedestals
 *
 * Revision 1.6  2011/10/25 20:43:32  ckriley
 * added more plots
 *
 * Revision 1.5  2011/10/25 14:46:22  ckriley
 * update for more plots
 *
 * Revision 1.4  2011/10/20 17:13:45  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 * Revision 1.3  2011/10/20 16:58:41  ckriley
 * test
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtEvent;
class StFgtDbMaker;
class StFgtPedPlotter;
class StFgtA2CMaker;
class StFgtCosmicTrackMaker;
class StFgtSimpleClusterAlgo;
class StFgtMaxClusterAlgo;
class StFgtSimplePointAlgo;
class StFgtSimpleCosmicTrackAlgo;
class StFgt2PointCosmicTrackAlgo;
class StFgtClusterMaker;
class StFgtPointMaker;
class StFgtCosmicTrackQA;
class StFgtCosmicTrackPlots;

StChain             *analysisChain = 0;
StFgtCosmicMaker    *cosmicMkr     = 0;
StFgtDbMaker        *dbMkr         = 0;
StFgtA2CMaker       *adcCorrector  = 0;
StFgtSimpleClusterAlgo *simpleClusterAlgo = 0;
StFgtMaxClusterAlgo    *maxClusterAlgo    = 0;
StFgtSimplePointAlgo   *simplePointAlgo   = 0;
StFgtSimpleCosmicTrackAlgo *simpleCosmicTrackAlgo   =0;
StFgt2PointCosmicTrackAlgo *twoPointCosmicTrackAlgo =0;
StFgtClusterMaker   *clusterMkr    = 0;
StFgtPointMaker     *pointMkr      = 0;
StFgtCosmicTrackMaker *trackMkr    = 0;

StFgtPedPlotter     *pedPlotter       = 0;
StFgtCosmicTrackQA      *cosmicQA1 = 0;
StFgtCosmicTrackQA      *cosmicQA2 = 0;
StFgtCosmicTrackQA      *cosmicQA3 = 0;
StFgtCosmicTrackPlots *cosmicHists = 0;
StFgtCosmicTrackPlots *cosmicHists = 0;
StFgtCosmicTrackPlots *cosmicHists = 0;

TCanvas             *can1  = 0;
TCanvas             *can2  = 0;
TCanvas             *can3  = 0;
TCanvas             *can4  = 0;
TCanvas             *can5  = 0;
TCanvas             *can6  = 0;
TCanvas             *can7  = 0;
TCanvas             *can8  = 0;
TCanvas             *can9  = 0;
TCanvas             *can10 = 0;
TCanvas             *can11 = 0;
TCanvas             *can12 = 0;

Int_t fgtCosmicTrackQA( const Char_t *filename = "testfile.sfs",
                        const Char_t *pedfilename = "testfile.Ped.txt",
                        const Char_t *statusfilename = "testfile.Status.txt",
                        Int_t disc1 = 0,
                        Int_t quad1 = 0,
                        Int_t disc2 = 0,
                        Int_t quad2 = 0,
                        Int_t disc3 = 0,
                        Int_t quad3 = 0,
                        const Char_t *runname = "testrun",
                        const Char_t *quadname1 = "010",
                        const Char_t *quadname2 = "011",
                        const Char_t *quadname3 = "013",
                        Int_t nevents = -1,
                        Int_t timeBin = 4,
                        Bool_t cutShortEvents = 1,
                        Bool_t skipCor = 1 ){
   LoadLibs();
   cout << "Loaded libraries" << endl;
   Int_t ierr = 0;

   // hardcoded options
   Int_t numDiscs = 3;
   Float_t sansPedXmax =  500;
   Float_t sansPedXmin = -200;
   Int_t sansPedXbins = (sansPedXmax-sansPedXmin)/5;

   // so that don't have to rebin later
   //Int_t binFactorXc = 8, binFactorXr = 2, binFactorXp = 4, binFactorY = 32;
   Int_t numChannelsPerCorBin = 1;

   // canvas size
   Int_t canWidth = 850, canHeight = 1100;

   Short_t timeBinMask = (1<<timeBin);

   cout << "Constructing the ped plotter" << endl;
   pedPlotter = new StFgtPedPlotter;
   pedPlotter->setReadFromFile( pedfilename );
   pedPlotter->setTimeBinMask( 1<<timeBin );
   pedPlotter->setPlotVsStrip( 'c' );
   pedPlotter->setDisc( disc2 );
   pedPlotter->setQuad( quad2 );
   pedPlotter->setQuadName( quadname2 );
   pedPlotter->setPlotStDev( 1 );

   cout << "making the ped. st. dev. plots" << endl;
   ierr = pedPlotter->makePlots();
   cout << "\tdone" << endl;

   if( ierr ){
      cerr << "\tError making pedestal st. dev. plot" << endl;
      return;
   }

   const TGraph* gr = pedPlotter->getGraph( timeBin );
   if( !gr ){
      cerr << "\tError making pedestal st. dev. plot" << endl;
      return;
   }

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename );

   cout << "Constructing the A2C maker" << endl;
   adcCorrector = new StFgtA2CMaker( "adcCorrector" );
   adcCorrector->setPedReaderFile( pedfilename );
   adcCorrector->setStatusReaderFile( statusfilename );
   adcCorrector->setTimeBinMask( timeBinMask );
   adcCorrector->setAbsThres( -10000 );  // set below -4096 to skip cut
   adcCorrector->setRelThres( 5 );       // set to zero to skip cut
   adcCorrector->doRemoveOtherTimeBins( 1 );

   cout << "Constructing the cluster and point maker " << endl;
   simpleClusterAlgo = new StFgtSimpleClusterAlgo();
   maxClusterAlgo = new StFgtMaxClusterAlgo();
   simplePointAlgo = new StFgtSimplePointAlgo();
   simpleCosmicTrackAlgo = new StFgtSimpleCosmicTrackAlgo();
   twoPointCosmicTrackAlgo = new StFgt2PointCosmicTrackAlgo();

   clusterMkr = new StFgtClusterMaker("clusterMkr" );
   clusterMkr->setClusterAlgo(simpleClusterAlgo);

   pointMkr = new StFgtPointMaker( "pointMkr" );
   pointMkr->setPointAlgo(simplePointAlgo);

   cout << "Constructing cosmic trackmaker" << endl;
   trackMkr = new StFgtCosmicTrackMaker( "trackMkr" );
   trackMkr->setCosmicTrackAlgo(simpleCosmicTrackAlgo);

   cout << "Constructing the QA makers" << endl;

   // StFgtA2CMaker needs to be made before these QA plots
   cosmicQA1 = new StFgtCosmicTrackQA( "cosmicQA1", disc1, quad1, quadname1 );
   cosmicQA1->setTimeBin( timeBin );
   cosmicQA1->setFilenameBase( "" );
   cosmicQA1->setXbins( sansPedXbins );
   cosmicQA1->setXrange( sansPedXmin, sansPedXmax );
   cosmicQA1->setBinFactors( 1, 1 );

   cosmicQA2 = new StFgtCosmicTrackQA( "cosmicQA2", disc2, quad2, quadname2 );
   cosmicQA2->setTimeBin( timeBin );
   cosmicQA2->setFilenameBase( "" );
   cosmicQA2->setXbins( sansPedXbins );
   cosmicQA2->setXrange( sansPedXmin, sansPedXmax );
   cosmicQA2->setBinFactors( 1, 1 );

   cosmicQA3 = new StFgtCosmicTrackQA( "cosmicQA3", disc3, quad3, quadname3 );
   cosmicQA3->setTimeBin( timeBin );
   cosmicQA3->setFilenameBase( "" );
   cosmicQA3->setXbins( sansPedXbins );
   cosmicQA3->setXrange( sansPedXmin, sansPedXmax );
   cosmicQA3->setBinFactors( 1, 1 );

   cosmicHistsTop = new StFgtCosmicTrackPlots( "fgtTrackQATop", "trackMkr", disc1, quad1, quadname1 );

   cosmicHists = new StFgtCosmicTrackPlots( "fgtTrackQA", "trackMkr", disc2, quad2, quadname2 );

   cosmicHistsBottom = new StFgtCosmicTrackPlots( "fgtTrackQABottom", "trackMkr", disc3, quad3, quadname3 );

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
         cout << "\n\nevent number " << i << endl;

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

//    cout << "Hist ptrs " << hAdcVsChan << ' ' << hAdcVsR << ' ' << hAdcVsPhi << ' ' << hAdcSansPedVsChan << ' '
//         << hAdcSansPedVsR << ' ' << hAdcSansPedVsPhi << endl;

   gROOT->SetStyle("Plain");
   gStyle->SetOptStat(0);
   gStyle->SetEndErrorSize(0);
   gStyle->SetTitleBorderSize(0);
   gStyle->SetTitleTextColor(kRed);
   gStyle->SetPalette(1);
   gStyle->SetOptDate(0);

   // For the quadrant look -> set quadrantOutline=true
   bool quadrantOutline = true;
   TArc *innerArc=0;
   TArc *outerArc=0;
   TLine *left=0;
   TLine *right=0;
   TLine *center=0;
   TLine *weird=0;
   if(quadrantOutline) {
     innerArc = new TArc(0,0,10.35+1.15,0.0,90.0);
     outerArc = new TArc(0,0,39.40-1.15,0.0,90.0);
     left = new TLine( 1.15, 10.35+1.15, 1.15, 39.40-1.15 );
     right = new TLine( 10.35+1.15, 1.15, 39.40-1.15, 1.15 );
     center = new TLine(
      (10.35+1.15)*cos( 0.785398163 ),
      (10.35+1.15)*sin( 0.785398163 ),
      (39.40-1.15)*cos( 0.785398163 ),
      (39.40-1.15)*sin( 0.785398163 )
     );
     weird = new TLine(
      (39.40-1.15)*cos( 0.190240888 ),
      (39.40-1.15)*sin( 0.190240888 ),
      (39.40-1.15)*cos( 0.891863248 ),
      (39.40-1.15)*sin( 0.891863248 )
     );
     innerArc->SetFillStyle(0);
     outerArc->SetFillStyle(0);
     innerArc->SetLineWidth(2);
     outerArc->SetLineWidth(2);
     left->SetLineWidth(2);
     right->SetLineWidth(2);
     center->SetLineWidth(2);
     weird->SetLineWidth(2);
   }
   
   //Float_t maxY = pedPlotter->getMaxY()*1.05;
   //TH2F *histC = new TH2F ("histC", "", 1, 0,   1280, 1, 0, 200 );

   gROOT->SetStyle("Plain");
   TCanvas *can1 = new TCanvas( "fgtQAcan1", "fgtQAcan1", canHeight, canHeight );

   can1->Divide(5,6);
   gPad->SetGridx(0);
   int can1pad = 1;
   for(int i=0;i<10;i++) { 
     TH1F* hAPV1 = cosmicQA1->getHistAPV(i);
     TH1F* hAPV2 = cosmicQA2->getHistAPV(i);
     TH1F* hAPV3 = cosmicQA3->getHistAPV(i);
     can1->cd(can1pad);
     gStyle->SetOptStat(100110);  // for plotting overflows
     hAPV1->Draw();
     can1->cd(can1pad+10);
     gStyle->SetOptStat(100110);
     hAPV2->Draw();
     can1->cd(can1pad+20);
     gStyle->SetOptStat(100110);
     hAPV3->Draw();
     can1pad++;
   }
   gPad->Update();

   TCanvas *can2 = new TCanvas( "fgtQAcan2", "fgtQAcan2", 1300, canWidth );
   TH1F* clusterDist1R = cosmicQA1->getHist1Dcluster(0);
   TH1F* clusterDist2R = cosmicQA2->getHist1Dcluster(0);
   TH1F* clusterDist3R = cosmicQA3->getHist1Dcluster(0);
   TH1F* clusterDist1P = cosmicQA1->getHist1Dcluster(1);
   TH1F* clusterDist2P = cosmicQA2->getHist1Dcluster(1);
   TH1F* clusterDist3P = cosmicQA3->getHist1Dcluster(1);

   gPad->SetGridx(0);
   can2->Divide(3,2);

   can2->cd(1);
   gStyle->SetOptStat(100110);  // for plotting overflows
   clusterDist1R->Draw();

   can2->cd(2);
   gStyle->SetOptStat(100110);
   clusterDist2R->Draw();

   can2->cd(3);
   gStyle->SetOptStat(100110);
   clusterDist3R->Draw();

   can2->cd(4);
   gStyle->SetOptStat(100110);   
   clusterDist1P->Draw();

   can2->cd(5);
   gStyle->SetOptStat(100110);   
   clusterDist2P->Draw();

   can2->cd(6);
   gStyle->SetOptStat(100110);   
   clusterDist3P->Draw();
   gPad->Update();

/*
   // look for hot strips
   TH1F* testHist1R = cosmicQA1->getTestHist(0);
   TH1F* testHist2R = cosmicQA2->getTestHist(0);
   TH1F* testHist3R = cosmicQA3->getTestHist(0);
   TH1F* testHist1P = cosmicQA1->getTestHist(1);
   TH1F* testHist2P = cosmicQA2->getTestHist(1);
   TH1F* testHist3P = cosmicQA3->getTestHist(1);

   gPad->SetGridx(0);
   can2->Divide(3,2);
   
   can2->cd(1);
   gStyle->SetOptStat(100000);  // for plotting overflows
   testHist1R->Draw();

   can2->cd(2);
   gStyle->SetOptStat(100000);
   testHist2R->Draw();

   can2->cd(3);
   gStyle->SetOptStat(100000);
   testHist3R->Draw();

   can2->cd(4);
   gStyle->SetOptStat(100000);   
   testHist1P->Draw();

   can2->cd(5);
   gStyle->SetOptStat(100000);   
   testHist2P->Draw();

   can2->cd(6);
   gStyle->SetOptStat(100000);   
   testHist3P->Draw();
   gPad->Update();
*/


   TCanvas *can3 = new TCanvas( "fgtQAcan3", "fgtQAcan3", 1300, canWidth );
   TH1F* clusterDist1 = cosmicQA1->getHist1Dclusters();
   TH1F* clusterDist2 = cosmicQA2->getHist1Dclusters();
   TH1F* clusterDist3 = cosmicQA3->getHist1Dclusters();

   gPad->SetGridx(0);
   can3->Divide(3,1);
   
   can3->cd(1);
   gStyle->SetOptStat(100110);   
   clusterDist1->Draw();

   can3->cd(2);
   gStyle->SetOptStat(100110);   
   clusterDist2->Draw();

   can3->cd(3);
   gStyle->SetOptStat(100110);   
   clusterDist3->Draw();
   gPad->Update();

   TCanvas *can4 = new TCanvas( "fgtQAcan4", "fgtQAcan4", canWidth, 1500 );
   TH2F* pointDist1 = cosmicQA1->getHist2Dpoint();
   TH2F* pointDist2 = cosmicQA2->getHist2Dpoint();
   TH2F* pointDist3 = cosmicQA3->getHist2Dpoint();

   gStyle->SetOptStat(0);
   gPad->SetGridx(0);
   gPad->SetGridy(0);
   can4->Divide(1,3);

   can4->cd(1);
   pointDist1->Draw("colz");
   if(quadrantOutline) {
     outerArc->Draw("only");
     innerArc->Draw("only");
     left->Draw();
     right->Draw();
     center->Draw();
     weird->Draw();
   }
   gPad->Update();
   TPaletteAxis *palette = (TPaletteAxis*)pointDist1->GetListOfFunctions()->FindObject("palette");
   if( palette )
      palette->SetLabelSize(0.035);
 
   can4->cd(2);
   pointDist2->Draw("colz");
   if(quadrantOutline) {
     outerArc->Draw("only");
     innerArc->Draw("only");
     left->Draw();
     right->Draw();
     center->Draw();
     weird->Draw();
   }
   gPad->Update();
   TPaletteAxis *palette = (TPaletteAxis*)pointDist2->GetListOfFunctions()->FindObject("palette");
   if( palette )
      palette->SetLabelSize(0.035);
   
   can4->cd(3);
   pointDist3->Draw("colz");
   if(quadrantOutline) {
     outerArc->Draw("only");
     innerArc->Draw("only");
     left->Draw();
     right->Draw();
     center->Draw();
     weird->Draw();
   }
   gPad->Update();
   TPaletteAxis *palette = (TPaletteAxis*)pointDist3->GetListOfFunctions()->FindObject("palette");
   if( palette )
      palette->SetLabelSize(0.035);


   TCanvas *can5 = new TCanvas( "fgtTrackQAcan5", "fgtTrackQAcan5", canWidth, canHeight );
   TH2F* trackDist = cosmicHists->getHistTracks();
 
   gPad->SetGridx(0);
   gPad->SetGridy(0);

   trackDist->Draw("colz");
   if(quadrantOutline) {
     outerArc->Draw("only");
     innerArc->Draw("only");
     left->Draw();
     right->Draw();
     center->Draw();
     weird->Draw();
   }
   gPad->Update();
   TPaletteAxis *palette = (TPaletteAxis*)trackDist->GetListOfFunctions()->FindObject("palette");
   if( palette ){
      palette->SetLabelSize(0.035);
      palette->SetX1NDC(0.87);
      palette->SetX2NDC(0.92);
   };

   TCanvas *can6 = new TCanvas( "fgtTrackQAcan6", "fgtTrackQAcan6", canWidth, canHeight ); 
   TProfile2D* chi2Dist = cosmicHists->getProfileChi2();

   gPad->SetGridx(0);
   gPad->SetGridy(0);
   
   chi2Dist->Draw("colz");
   if(quadrantOutline) {
     outerArc->Draw("only");
     innerArc->Draw("only");
     left->Draw();
     right->Draw();
     center->Draw();
     weird->Draw();
   }
   gPad->Update();
   TPaletteAxis *palette = (TPaletteAxis*)chi2Dist->GetListOfFunctions()->FindObject("palette");
   if( palette ){
      palette->SetLabelSize(0.035);
      palette->SetX1NDC(0.87);
      palette->SetX2NDC(0.92);
   };

   TCanvas *can7 = new TCanvas( "fgtTrackQAcan7", "fgtTrackQAcan7", canWidth, canHeight );
   TProfile2D* efficiencyDist = cosmicHists->getProfileEfficiency();

   gPad->SetGridx(0);
   gPad->SetGridy(0);

   efficiencyDist->Draw("colz");
   if(quadrantOutline) {
     outerArc->Draw("only");
     innerArc->Draw("only");
     left->Draw();
     right->Draw();
     center->Draw();
     weird->Draw();
   }
   gPad->Update();
   TPaletteAxis *palette = (TPaletteAxis*)efficiencyDist->GetListOfFunctions()->FindObject("palette");
   if( palette ){
     palette->SetLabelSize(0.035);
     palette->SetX1NDC(0.87);
     palette->SetX2NDC(0.92);
   }

   TCanvas *can8 = new TCanvas( "fgtQAcan8", "fgtQAcan8", 1300, canWidth );
   TH1F* clusterDistTrack1R = cosmicHistsTop->getHist1Dcluster(0);
   TH1F* clusterDistTrack1P = cosmicHistsTop->getHist1Dcluster(1);
   TH1F* clusterDistTrack2R = cosmicHists->getHist1Dcluster(0);
   TH1F* clusterDistTrack2P = cosmicHists->getHist1Dcluster(1);
   TH1F* clusterDistTrack3R = cosmicHistsBottom->getHist1Dcluster(0);
   TH1F* clusterDistTrack3P = cosmicHistsBottom->getHist1Dcluster(1);
   
   gPad->SetGridx(0);
   can8->Divide(3,2);
   
   can8->cd(1);
   gStyle->SetOptStat(100110);
   clusterDistTrack1R->Draw();
   
   can8->cd(2);
   gStyle->SetOptStat(100110);
   clusterDistTrack2R->Draw();

   can8->cd(3);
   gStyle->SetOptStat(100110);
   clusterDistTrack3R->Draw();

   can8->cd(4);
   gStyle->SetOptStat(100110);
   clusterDistTrack1P->Draw();

   can8->cd(5);
   gStyle->SetOptStat(100110);
   clusterDistTrack2P->Draw();

   can8->cd(6);
   gStyle->SetOptStat(100110);
   clusterDistTrack3P->Draw();
   gPad->Update();


   TCanvas *can9 = new TCanvas( "fgtQAcan9", "fgtQAcan9", 1300, canWidth );
   TH1F* clusterDistTrack1 = cosmicHistsTop->getHist1Dclusters();
   TH1F* clusterDistTrack2 = cosmicHists->getHist1Dclusters();
   TH1F* clusterDistTrack3 = cosmicHistsBottom->getHist1Dclusters();

   gPad->SetGridx(0);
   can9->Divide(3,1);

   can9->cd(1);
   gStyle->SetOptStat(100110);
   clusterDistTrack1->Draw();

   can9->cd(2);
   gStyle->SetOptStat(100110);
   clusterDistTrack2->Draw();

   can9->cd(3);
   gStyle->SetOptStat(100110);
   clusterDistTrack3->Draw();
   gPad->Update();


   TCanvas *can10 = new TCanvas( "fgtTrackQAcan10", "fgtTrackQAcan10", canWidth, canHeight );
   TH2F* realTrackPointDist = cosmicHists->getHist2Dcluster();

   gPad->SetGridx(0);
   gPad->SetGridy(0);
   gStyle->SetOptStat(0);

   realTrackPointDist->Draw("colz");
   if(quadrantOutline) {
     outerArc->Draw("only");
     innerArc->Draw("only");
     left->Draw();
     right->Draw();
     center->Draw();
     weird->Draw();
   }


   TCanvas *can11 = new TCanvas( "fgtTrackQAcan11", "fgtTrackQAcan11", canWidth, canHeight );
   TH2F* realtrackDist = cosmicHists->getHistRealTracks();
   
   gPad->SetGridx(0);
   gPad->SetGridy(0);
   
   realtrackDist->Draw("colz");
   if(quadrantOutline) {
     outerArc->Draw("only");
     innerArc->Draw("only");
     left->Draw();
     right->Draw();
     center->Draw();
     weird->Draw();
   }
   gPad->Update();
   TPaletteAxis *palette = (TPaletteAxis*)realtrackDist->GetListOfFunctions()->FindObject("palette");
   if( palette ){
      palette->SetLabelSize(0.035);
      palette->SetX1NDC(0.87);
      palette->SetX2NDC(0.92);
   };


   TCanvas *can12 = new TCanvas( "fgtTrackQAcan12", "fgtTrackQAcan12", canWidth, canHeight );
   //TProfile* dXvX = cosmicHists->getProfileX();
   //dXvX->Draw();
   TCanvas *can13 = new TCanvas( "fgtTrackQAcan13", "fgtTrackQAcan13", canWidth, canHeight );
   //TProfile* dYvY = cosmicHists->getProfileY();
   //dYvY->Draw();
 

   can12->Divide(4,4);
   can13->Divide(4,4);
   int canpad=1;
   for(int i=3;i>=0;i--) {
     for(int j=0;j<4;j++) {
       can12->cd(canpad);
       TH1F* hdX = cosmicHists->getHistX(i,j);
       hdX->Draw();
       can13->cd(canpad);
       TH1F* hdY = cosmicHists->getHistY(i,j);
       hdY->Draw();
       canpad++;
     }
   } 

   cout << "ready to save plots" << endl;

   std::string fileOut1  = "fgtCosmicTrackQA.page-1.";
   std::string fileOut2  = "fgtCosmicTrackQA.page-2.";
   std::string fileOut3  = "fgtCosmicTrackQA.page-3.";
   std::string fileOut4  = "fgtCosmicTrackQA.page-4.";
   std::string fileOut5  = "fgtCosmicTrackQA.page-5.";
   std::string fileOut6  = "fgtCosmicTrackQA.page-6.";
   std::string fileOut7  = "fgtCosmicTrackQA.page-7.";
   std::string fileOut8  = "fgtCosmicTrackQA.page-8.";
   std::string fileOut9  = "fgtCosmicTrackQA.page-9.";
   std::string fileOut10 = "fgtCosmicTrackQA.page-10.";
   std::string fileOut11 = "fgtCosmicTrackQA.page-11.";
   std::string fileOut12 = "fgtCosmicTrackQA.page-12.";
   std::string fileOut13 = "fgtCosmicTrackQA.page-13.";

   std::string tail = runname;
   tail += ".fgt";
   tail += quadname2;
   tail += ".pdf";

   //cout << "\trunname is " << runname << endl;
   cout << "\tsaving to '" << tail << "'" << endl;
   can1->Print( (fileOut1+tail).data() );
   can2->Print( (fileOut2+tail).data() );
   can3->Print( (fileOut3+tail).data() );
   can4->Print( (fileOut4+tail).data() );
   can5->Print( (fileOut5+tail).data() );
   can6->Print( (fileOut6+tail).data() );
   can7->Print( (fileOut7+tail).data() );
   can8->Print( (fileOut8+tail).data() );
   can9->Print( (fileOut9+tail).data() );
   can10->Print((fileOut10+tail).data());
   can11->Print((fileOut11+tail).data());
   can12->Print((fileOut12+tail).data());
   can13->Print((fileOut13+tail).data());

   cout << "all done" << endl;
   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries

  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("StEvent");
  cout << "loaded StEvent library" << endl;

  gSystem->Load("RTS");
  gSystem->Load("StFgtDbMaker");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtStatusMaker");
  gSystem->Load("StFgtCosmicTrackMaker");
  gSystem->Load("StFgtQaMakers");
  gSystem->Load("StFgtPedPlotter");
  gSystem->Load("StFgtClusterMaker");
  gSystem->Load("StFgtPointMaker");
  gSystem->Load("StFgtA2CMaker");
};
