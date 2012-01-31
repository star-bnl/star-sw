/***************************************************************************
 *
 * $Id: makeADCplot.C,v 1.1 2012/01/31 09:26:18 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Script to make the basic ADC vs channel or ADC vs strip plots.
 *
 ***************************************************************************
 *
 * $Log: makeADCplot.C,v $
 * Revision 1.1  2012/01/31 09:26:18  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.1  2011/09/30 19:05:32  sgliske
 * general update
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtCorAdcMaker;
class StFgtQaAdcVsChannel;

StChain             *analysisChain = 0;
StFgtCosmicMaker    *cosmicMkr     = 0;
StFgtCorAdcMaker    *adcCorrector  = 0;
StFgtQaAdcVsChannel *adcPlotter    = 0;

TCanvas *can = 0;

Int_t makeADCplot( const Char_t *filename = "testfile.sfs",
                   const Char_t *pedfilename = "testfile.Ped.txt",
                   Int_t disc = 0,
                   Int_t quad = 0,
                   Char_t plotVsStrip = 'c',
                   Int_t subtrPeds = 1,
                   const Char_t *runname = "testrun",
                   const Char_t *quadname = "010",
                   Int_t nevents = -1,
                   Int_t timeBin = 4,
                   Int_t numDiscs = 3 ){
   LoadLibs();
   Int_t ierr = 0;

   Short_t timeBinMask = (1<<timeBin);

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename );
   cosmicMkr->setNumDiscs( numDiscs );

   cout << "Constructing the ADC corrector" << endl;

   // useful for removing extra time bins even if not ped subtracting

   adcCorrector = new StFgtCorAdcMaker( "adcCorrector", "cosmicMaker" );
   adcCorrector->setPedReaderFile( pedfilename );
   adcCorrector->setTimeBinMask( timeBinMask );
   adcCorrector->setAbsThres( -10000 );  // set below -4096 to skip cut
   adcCorrector->setRelThres( 0 );       // set to zero to skip cut
   adcCorrector->doSubtrPeds( subtrPeds );
   adcCorrector->doRemoveOtherTimeBins( 1 );

   cout << "Constructing the QA maker" << endl;

   // ranges and number of strips/channels to average over
   Int_t binFactorX = 8, binFactorY = 32;
   Float_t sansPedYmax = 1024;
   Float_t sansPedYmin = -512;
   Int_t sansPedYbins = 4096/binFactorY;

   if( plotVsStrip == 'R' || plotVsStrip == 'r' )
      binFactorX = 1;
   else if ( plotVsStrip == 'P' )
      binFactorX = 4;

   if( subtrPeds )
      binFactorY = 1;

   adcPlotter = new StFgtQaAdcVsChannel( "fgtQaAdc_1", "cosmicMaker", disc, quad, quadname );
   adcPlotter->setTimeBin( timeBin );
   adcPlotter->setFilenameBase( "" );     // do not save
   adcPlotter->setToPlotVsStrip( plotVsStrip );
   adcPlotter->setToSubtrPeds( 0 );       // already done
   adcPlotter->setBinFactors( binFactorX, binFactorY );
   if( subtrPeds ){
      adcPlotter->setYbins( sansPedYbins );
      adcPlotter->setYrange( sansPedYmin, sansPedYmax );
   };

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

      if( (i+1)%1000 == 1 )
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
   TH2F* hAdc        = adcPlotter->getHist();       

   gROOT->SetStyle("Plain");
   gStyle->SetOptStat(0);
   gStyle->SetEndErrorSize(0);
   gStyle->SetTitleBorderSize(0);
   gStyle->SetTitleTextColor(kRed);

   Int_t width = 1800;
   Int_t height = 600;
   if( plotVsStrip == 'R' || plotVsStrip == 'r' || plotVsStrip == 'P' ){
      width /= 3;
      height = width+1;
   };

   gROOT->SetStyle("Plain");
   TCanvas *can = new TCanvas( "fgtQAcan", "Fgt Cosmic QA", width, height );

   gStyle->SetOptStat(0);
   gStyle->SetPalette(1);

   //can->SetRightMargin( 0.02 );
   //can->SetLeftMargin( 0.13 );
   //hAdc->GetYaxis()->SetTitleOffset( 1.55 );

   hAdc->SetMaximum( subtrPeds ? 100 : 200 );
   hAdc->Draw("COLZ");

   cout << "ready to save" << endl;

   std::string fileOut = "fgtCQA.ADCplot.";
   fileOut += runname;
   fileOut += ".fgt";
   fileOut += quadname;
   fileOut += ".eps";

   cout << "\tsaving to '" << fileOut << "'" << endl;
   can->Print( fileOut.data() );

   TFile *f = new TFile ("testfile.root", "RECREATE");
   hAdc->Write();
   f->Close();

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
  gSystem->Load("StFgtCorAdcMaker");

};
