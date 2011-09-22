/***************************************************************************
 *
 * $Id: plotPedsFromFile.C,v 1.1 2011/09/22 21:22:03 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Plots peds from a file
 *
 ***************************************************************************
 *
 * $Log: plotPedsFromFile.C,v $
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
                      Short_t timeBin = 4,
                      Bool_t plotVsStrip = 1 ){
   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the ped plotter" << endl;
   pedPlotter = new StFgtPedPlotter;
   pedPlotter->setReadFromFile( filenameIn );
   pedPlotter->setTimeBinMask( 1<<timeBin );
   pedPlotter->setPlotVsStrip( plotVsStrip );
   pedPlotter->setDisc( disc );
   pedPlotter->setQuad( quad );

   cout << "making the graphs" << endl;
   ierr = pedPlotter->makePlots();

   if( ierr ){
      cerr << "Error making plots" << endl;
   } else {
      gROOT->SetStyle("Plain");
      gStyle->SetOptStat(0);
      gStyle->SetEndErrorSize(0);
      gStyle->SetTitleBorderSize(0);
      gStyle->SetTitleTextColor(kRed);

      can = new TCanvas("can","Pedistal Canvas",900,400);
      can->SetRightMargin( 0.02 );

      Int_t width = 1280;
      if( plotVsStrip )
         width = 2*720;

      //Float_t maxX = pedPlotter->getMaxX();
      Float_t maxY = pedPlotter->getMaxY()*1.05;

      TH2F *hist = new TH2F ("hist", "", 1, 0, width, 1, 0, maxY );
      hist->Draw("H");

      const TGraphErrors* gr = pedPlotter->getGraph( timeBin );
      if( gr ){
         gr->SetLineColor( kBlue );
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

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtPedPlotter");
};
