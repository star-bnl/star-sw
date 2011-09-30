/***************************************************************************
 *
 * $Id: plotPedsFromFile.C,v 1.5 2011/09/30 19:09:07 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Plots peds from a file
 *
 ***************************************************************************
 *
 * $Log: plotPedsFromFile.C,v $
 * Revision 1.5  2011/09/30 19:09:07  sgliske
 * general update
 *
 * Revision 1.4  2011/09/29 18:39:43  sgliske
 * Update for geoId->elecCoord function now in StFgtCosmicTestStandGeom
 *
 * Revision 1.3  2011/09/27 00:49:00  sgliske
 * cosmic QA update
 *
 * Revision 1.2  2011/09/26 16:55:52  sgliske
 * Continued work on cosmic QA plots
 *
 * Revision 1.1  2011/09/22 21:22:03  sgliske
 * creation
 *
 *
 **************************************************************************/

// forward declarations
class StFgtPedPlotter;

StFgtPedPlotter *pedPlotter = 0;
TCanvas *can                = 0;

int plotPedsFromFile( const Char_t *filenameIn = "testfile.Ped.txt",
                      const Char_t *filenameOut = "testfile.Ped.eps",
                      Short_t disc = 0,
                      Short_t quad = 0,
                      Char_t* quadName = "FGT #010",
                      Char_t plotVsStrip = 'c',
                      Bool_t doPlotStDev = 1,
                      Short_t timeBin = 4 ){
   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the ped plotter" << endl;
   pedPlotter = new StFgtPedPlotter;
   pedPlotter->setReadFromFile( filenameIn );
   pedPlotter->setTimeBinMask( 1<<timeBin );
   pedPlotter->setPlotVsStrip( plotVsStrip );
   pedPlotter->setDisc( disc );
   pedPlotter->setQuad( quad );
   pedPlotter->setPlotStDev( doPlotStDev );

   cout << "making the graphs" << endl;
   ierr = pedPlotter->makePlots();
   cout << "\tdone" << endl;

   if( ierr ){
      cerr << "Error making plots" << endl;
   } else {
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

      can = new TCanvas("can","Pedistal Canvas",width,height);
      can->SetRightMargin( 0.02 );
      can->SetGridx(0);
      can->SetGridy(0);

      Float_t xlow = 0;
      Float_t xhigh = 1280;
      if( plotVsStrip == 'R' ){
         xlow = 10;
         xhigh = 40;
      } else if ( plotVsStrip == 'P' ){
         xlow = 0;
         xhigh = 6.28/4;
      };

      //Float_t maxX = pedPlotter->getMaxX();
      Float_t maxY = pedPlotter->getMaxY()*1.05;
      if( maxY < 512 )
         maxY = 512;

      TH2F *hist = new TH2F ("hist", "", 1, xlow, xhigh, 1, 0, maxY );
      hist->Draw("H");

      const TGraphErrors* gr = pedPlotter->getGraph( timeBin );
      if( gr ){
         gr->SetLineColor( kBlue );
         gr->SetMarkerStyle( 20 );
         gr->SetMarkerSize( 0.4 );
         if( doPlotStDev )
            gr->SetMarkerColor( kBlue );
         gr->Draw("PE same");
         hist->SetTitle( gr->GetTitle() );
      };

      std::string title = hist->GetTitle();
      title += std::string(", ") + quadName;
      hist->SetTitle( title.data() );

      if( filenameOut )
         can->Print( filenameOut );
   };

   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries

//   gSystem->Load("libPhysics");
//   gSystem->Load("St_base");
//   gSystem->Load("StChain");
//   gSystem->Load("StEvent");
//   cout << "loaded StEvent library" << endl;

  gSystem->Load("StUtilities");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtPedPlotter");
};
