/***************************************************************************
 *
 * $Id: StSvtGuiMonitor.cc,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT GUI Monitor
 *
 ***************************************************************************
 *
 * $Log: StSvtGuiMonitor.cc,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#include "StSvtGuiMonitor.hh"
#include "StSvtMenuBar.hh"
#include "StSvtView.hh"
#include "StSvtMonitor.hh"
 
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtHybridHist.hh"
#include "StSvtHybridHist2D.hh"
#include "StSvtHybridHistAnalog.hh"
#include "StSvtHist.hh"
#include "StSvtHist2D.hh"
#include "StSvtHistAnalog.hh"
#include "StSvtHybridGraph.hh"
#include "StSvtGraph.hh"

#include "StSvtClusterMaker/StSvtAnalysedHybridClusters.hh"
#include "StarClassLibrary/StThreeVectorD.hh"

#include <TGCanvas.h>
#include <TRootEmbeddedCanvas.h>
#include <TCanvas.h>
#include <TDialogCanvas.h>
#include "TOrdCollection.h"
#include "TH1.h"
#include "TH2.h"
#include "TAxis.h"
#include "TGraph.h"
#include "TStyle.h"
#include "TPaveText.h"
#include "TEllipse.h"

// In the future, get this Data base
#define N_ANODES 240
#define N_TIMEBINS 128
#define MAX_NUMBER_OF_EVENTS 100
#define MAX_NUMBER_OF_HISTOGRAMS 100
#define N_BINS_ANODES 120
#define N_BINS_TIME 64
// end

float maxADC, minADC;
TOrdCollection* histograms;
int nEvents, hybridID, waferID, ladderID, barrelID, histID;

ClassImp(StSvtGuiMonitor)

StSvtGuiMonitor::StSvtGuiMonitor(char* config, StChain* chain, StChain* bfchain, const TGWindow *p, UInt_t w, UInt_t h)
      : TGMainFrame(p, w, h)
{ 
  // Instantiate menu bar
  fMenuBar = new StSvtMenuBar(this, 1, 1, kHorizontalFrame);
  fMenuBarLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX,
				     0, 0, 1, 1);
  AddFrame(fMenuBar, fMenuBarLayout);
       
  // Instantiate Canvas
  UInt_t Width;
  UInt_t Height;
  if ((!strncmp(config,"SYST", strlen("SYST"))) || (!strncmp(config,"Y1L", strlen("Y1L")))) {
    Width = 500 ;
    Height = 300 ;
  } 
  else {
    Width = 470;
    Height = 435; 
  }
  fCanvas = new TRootEmbeddedCanvas("canvas",this,Width,Height);

  // Instantiate SVT view
  TDialogCanvas *dcan = (TDialogCanvas*)fCanvas->GetCanvas(); 
  dcan->SetEditable(kFALSE);
  fSvtView = new StSvtView(config,dcan);
   
  // Process window layout
  AddFrame(fCanvas,new TGLayoutHints( kLHintsExpandX | kLHintsExpandY, 0,0,2,2));  
  SetWindowName("On Line Monitor");
  MapSubwindows();
  // we need to use GetDefault...() to initialize the layout algorithm...
  Resize(GetDefaultSize());
  //Resize(400, 200);
  MapWindow();

  // Instantiate Svt Monitor
  fSvtMonitor = new StSvtMonitor(config);

  // Make links for Menu Bar
  fMenuBar->SetSvtGuiMonitor(this);
  fMenuBar->SetSvtMonitor(fSvtMonitor);
  fMenuBar->SetChain(chain,bfchain);

  // Initialize Menu Bar
  fMenuBar->ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),0,0);

  // Initialize projection collection
  histProj = new TOrdCollection;

  mPadLadder = new TPad*[14];
  mPadBarrel = new TPad*[16];
}
 
StSvtGuiMonitor::~StSvtGuiMonitor()
{
   // Delete all created widgets.

   delete fCanvas; 
   delete fSvtView;
   delete fSvtMonitor;

   delete fMenuBar;
   delete fMenuBarLayout;
}

Bool_t StSvtGuiMonitor::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{
   // Handle messages send to the TestMainFrame object. E.g. all menu button
   // messages.
  return fMenuBar->ProcessMessage(this, msg, parm1, parm2);
}

//*****************************************************************************

void StSvtGuiMonitor::drawHist(char* option, char* type)
{
  histograms = fSvtMonitor->getHistograms();
  hybridID = fSvtMonitor->getHybridID();
  waferID = fSvtMonitor->getWaferID();
  ladderID = fSvtMonitor->getLadderID();
  barrelID = fSvtMonitor->getBarrelID();
  histID = fSvtMonitor->getHistID();
  nEvents = fSvtMonitor->getnEvents();

  int index_hyb, indexHist;
  char drawOption[20];
  TString className, config="NULL";

  StSvtHist* svtHist;
  StSvtHybridHist* hybridHist;

  if ((nEvents == 0) && (histID < 300)) {
    cout << "No event in buffer! Get one..." << endl;
    return;
  }

  indexHist = fSvtMonitor->getHistIndex(histID);

  if (indexHist >= 0) {

    className = TString(histograms->At(indexHist)->ClassName());

    if ((className != "TH1F") && (className != "TH2F") && (className != "TGraph")) {
      svtHist = (StSvtHist*)histograms->At(indexHist);
      if (svtHist) {
	config = TString(svtHist->getConfiguration());
	index_hyb = svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID);
	hybridHist = (StSvtHybridHist*)svtHist->at(index_hyb);
      }
      else {
	cout << "Not a valid HISTOGRAM!!!" << endl;
	return;
      }
    }
  }
  else {
    cout << "No histogram with ID = " << histID << endl;
    return;
  }
    
  TH1* hist;

  if (className == "StSvtHist2D") {
    hist = (TH2F*)hybridHist->getHist() ;
    sprintf(drawOption,"COLZ");
    gStyle->SetOptStat(0);
  }
  else if (className == "StSvtHist") {
    hist = hybridHist->getHist() ;
    if (TString(option) == "same")
      sprintf(drawOption,"SAME");
    else
      sprintf(drawOption,"H");
    gStyle->SetOptStat(1111);
  }
  else if (className == "StSvtHistAnalog") {
    hist = ((StSvtHybridHistAnalog*)hybridHist)->getHist(1) ;
  }
  else if (className == "StSvtGraph") {
    hist = 0;
    fSvtMonitor->fillHist();    
  }
  else if (className == "TH1F") {
    hist = (TH1F*)histograms->At(indexHist);
    if (TString(option) == "same")
      sprintf(drawOption,"SAME");
    else
      sprintf(drawOption,"H");
    gStyle->SetOptStat(1111);
  }
  else if (className == "TH2F") {
    hist = (TH2F*)histograms->At(indexHist);
    sprintf(drawOption,"COLZ");
    gStyle->SetOptStat(0);
  }
  else if (className == "TGraph") {
    ((TGraph*)histograms->At(indexHist))->SetMarkerStyle(20);
    ((TGraph*)histograms->At(indexHist))->Draw("AP");
    hist = ((TGraph*)histograms->At(indexHist))->GetHistogram();
    gStyle->SetOptStat(0);
    return;
  }

  if (config == "BARREL") {
    if (TString(option) == "same")
      sprintf(drawOption,"PSAME");
    else
      sprintf(drawOption,"P");
    gStyle->SetOptStat(0);
    hist->SetMarkerStyle(20);

    switch (barrelID) {
    case 1:
      hist->SetAxisRange(0,8);
      break;
    case 2:
      hist->SetAxisRange(0,12);
      break;
    case 3:
      hist->SetAxisRange(0,16);
      break;
    }
  }

  if (config == "SVT") {
    if (TString(option) == "same")
      sprintf(drawOption,"PSAME");
    else
      sprintf(drawOption,"P");
    gStyle->SetOptStat(0);
    hist->SetMarkerStyle(20);

    switch (barrelID) {
    case 1:
      if (hist->GetXaxis()->GetXmax() < 33)
	hist->SetAxisRange(0,16);
      else
	hist->SetAxisRange(0,64);
      break;
    case 2:
      if (hist->GetXaxis()->GetXmax() < 33)
	hist->SetAxisRange(0,24);
      else
	hist->SetAxisRange(0,144);
      break;
    case 3:
      if (hist->GetXaxis()->GetXmax() < 33)
	hist->SetAxisRange(0,32);
      else
	hist->SetAxisRange(0,224);
      break;
    }
  }

  // plot histograms
  if (hist) {

    if ((!hist->GetEntries()) && (histID < 200))
      fSvtMonitor->fillHist();
    //if ((histID > 100) && (histID < 200))
    //  fSvtMonitor->fillHist();    
    if ((!hist->GetEntries()) && (histID > 300))
      fSvtMonitor->fillHistPed();
    
    if ( (TString(type) == "LADDER") || (TString(type) == "BARREL") ) {
      gStyle->SetOptStat(0);
      gStyle->SetOptTitle(0);
      gStyle->SetStatH(.2);
      gStyle->SetStatW(.3);
      hist->GetXaxis()->SetTitleSize(.07);
      hist->GetYaxis()->SetTitleSize(.07);
      hist->SetLabelSize(.07,"X");
      hist->SetLabelSize(.07,"Y");
      hist->SetNdivisions(406);

      hist->SetMaximum(maxADC);
      hist->SetMinimum(minADC);
      if (TString(option) == "same") {
	if ((TString(type) == "LADDER") && (waferID == 1) && (hybridID == 1))	
	  ++mColorIndex;
	if ((TString(type) == "BARREL") && (ladderID == 1))	
	  ++mColorIndex;
      }
      else
	mColorIndex = 1;
   }
    else {
      gStyle->SetTitleW(.75);
      gStyle->SetOptTitle(1);
      gStyle->SetStatH();
      gStyle->SetStatW();
      hist->GetXaxis()->SetTitleSize(.04);
      hist->GetYaxis()->SetTitleSize(.04);
      hist->SetLabelSize(.04,"X");
      hist->SetLabelSize(.04,"Y");
      hist->SetNdivisions(510);
      hist->SetMaximum();
      hist->SetMinimum();
      if (TString(option) == "same")
	++mColorIndex;
      else
	mColorIndex = 1;
    }

    hist->SetLineColor(mColorIndex);
    hist->SetMarkerColor(mColorIndex);
  }    

  if ( !strncmp(option, "NULL", strlen("NULL")) )
    return;

  if (className== "StSvtHistAnalog")
    ((StSvtHybridHistAnalog*)hybridHist)->Draw(option);
  else if (className == "StSvtGraph")
    ((StSvtHybridGraph*)hybridHist)->Draw(option);
  else if (histID == 10)
    hist->Draw(option);
  else
    hist->DrawCopy(drawOption);

  TLine *line = new TLine();
  line->SetLineStyle(2);

  if (config == "BARREL") {
    switch (barrelID) {
    case 1:
      line->DrawLine(4.5,0,4.5,hist->GetMaximum());
      break;
    case 2:
      line->DrawLine(6.5,0,6.5,hist->GetMaximum());
      break;
    case 3:
      if (ladderID%2)
	line->DrawLine(6.5,0,6.5,hist->GetMaximum());
      else
	line->DrawLine(8.5,0,8.5,hist->GetMaximum());
      break;
    }
  }

  if (config == "SVT") {
    switch (barrelID) {
    case 1:
      if (hist->GetXaxis()->GetXmax() < 33)
	line->DrawLine(8.5,0,8.5,hist->GetMaximum());
      else {
	line->DrawLine(8.5,0,8.5,hist->GetMaximum());
	line->DrawLine(16.5,0,16.5,hist->GetMaximum());
	line->DrawLine(24.5,0,24.5,hist->GetMaximum());
	line->DrawLine(32.5,0,32.5,hist->GetMaximum());
	line->DrawLine(40.5,0,40.5,hist->GetMaximum());
	line->DrawLine(48.5,0,48.5,hist->GetMaximum());
	line->DrawLine(56.5,0,56.5,hist->GetMaximum());
      }
      break;
    case 2:
      if (hist->GetXaxis()->GetXmax() < 33)
	line->DrawLine(12.5,0,12.5,hist->GetMaximum());
      else {
	line->DrawLine(12.5,0,12.5,hist->GetMaximum());
	line->DrawLine(24.5,0,24.5,hist->GetMaximum());
	line->DrawLine(36.5,0,36.5,hist->GetMaximum());
	line->DrawLine(48.5,0,48.5,hist->GetMaximum());
	line->DrawLine(60.5,0,60.5,hist->GetMaximum());
	line->DrawLine(72.5,0,72.5,hist->GetMaximum());
	line->DrawLine(84.5,0,84.5,hist->GetMaximum());
	line->DrawLine(96.5,0,96.5,hist->GetMaximum());
	line->DrawLine(108.5,0,108.5,hist->GetMaximum());
	line->DrawLine(120.5,0,120.5,hist->GetMaximum());
	line->DrawLine(132.5,0,132.5,hist->GetMaximum());
      }
      break;
    case 3:
      if (hist->GetXaxis()->GetXmax() < 33) {
	line->DrawLine(16.5,0,16.5,hist->GetMaximum());
      }
      else {
	line->DrawLine(14.5,0,14.5,hist->GetMaximum());
	line->DrawLine(28.5,0,28.5,hist->GetMaximum());	
	line->DrawLine(42.5,0,42.5,hist->GetMaximum());	
	line->DrawLine(56.5,0,56.5,hist->GetMaximum());	
	line->DrawLine(70.5,0,70.5,hist->GetMaximum());	
	line->DrawLine(84.5,0,84.5,hist->GetMaximum());	
	line->DrawLine(98.5,0,98.5,hist->GetMaximum());	
	line->DrawLine(112.5,0,112.5,hist->GetMaximum());	
	line->DrawLine(126.5,0,126.5,hist->GetMaximum());	
	line->DrawLine(140.5,0,140.5,hist->GetMaximum());	
	line->DrawLine(154.5,0,154.5,hist->GetMaximum());	
	line->DrawLine(168.5,0,168.5,hist->GetMaximum());	
	line->DrawLine(182.5,0,182.5,hist->GetMaximum());	
	line->DrawLine(196.5,0,196.5,hist->GetMaximum());	
	line->DrawLine(210.5,0,210.5,hist->GetMaximum());	
      }
     break;
    }
  }
}

void StSvtGuiMonitor::drawHistLadder(char* option, TCanvas* canvas, int firstybin, int lastybin)
{
  histograms = fSvtMonitor->getHistograms();
  nEvents = fSvtMonitor->getnEvents();
  ladderID = fSvtMonitor->getLadderID();
  barrelID = fSvtMonitor->getBarrelID();
  histID = fSvtMonitor->getHistID();
  int h = fSvtMonitor->getHybridID();
  int w = fSvtMonitor->getWaferID();

  float xlow[7], xup[7], ylow[2], yup[2];
  int index_hyb, indexHist, numberOfWafers, numberOfHybrids=2, ipad;
  int barrel, ladder;
  char append[10], title[10];
  TString config;

  StSvtHist* svtHist;

  switch(barrelID) {
    
  case 1:
    xlow[0] = 0.;
    xup[0]  = 0.265;
    xlow[1] = 0.265;
    xup[1]  = 0.50;
    xlow[2] = 0.50;
    xup[2]  = 0.735;
    xlow[3] = 0.735;
    xup[3]  = 1.;
    numberOfWafers = 4;
    break;

  case 2:
    xlow[0] = 0.;
    xup[0]  = 0.2;
    xlow[1] = 0.2;
    xup[1]  = 0.36;
    xlow[2] = 0.36;
    xup[2]  = 0.52;
    xlow[3] = 0.52;
    xup[3]  = 0.68;
    xlow[4] = 0.68;
    xup[4]  = 0.84;
    xlow[5] = 0.84;
    xup[5]  = 1.;
    numberOfWafers = 6;
    break;

  case 3:
    xlow[0] = 0.;
    xup[0]  = 0.16;
    xlow[1] = 0.16;
    xup[1]  = 0.3;
    xlow[2] = 0.3;
    xup[2]  = 0.44;
    xlow[3] = 0.44;
    xup[3]  = 0.58;
    xlow[4] = 0.58;
    xup[4]  = 0.72;
    xlow[5] = 0.72;
    xup[5]  = 0.86;
    xlow[6] = 0.86;
    xup[6]  = 1.;
    numberOfWafers = 7;
    break;
  }

  ylow[0] = 0.;
  yup[0]  = 0.55;
  ylow[1] = 0.55;
  yup[1]  = 1.;
      
  gStyle->SetPadBottomMargin(0.15);
  gStyle->SetPadTopMargin(0.01);
  gStyle->SetPadRightMargin(0.01);
  gStyle->SetPadLeftMargin(0.2);

  maxADC = -999; 
  minADC = 999; 

  indexHist = fSvtMonitor->getHistIndex(histID);

  if (indexHist >= 0) {

    const char* className = histograms->At(indexHist)->ClassName();
	  
    svtHist = (StSvtHist*)histograms->At(indexHist) ;
    if (!svtHist) {
      cout << "Not a valid HISTOGRAM!!!" << endl;
      return;
    }
    
    if (TString(className) != "StSvtGraph") {

      for (int ihybrid=0;ihybrid < numberOfHybrids; ihybrid++) {
	
	hybridID = ihybrid + 1;
	
	for (int iwafer=0;iwafer < numberOfWafers; iwafer++) {      
	  
	  waferID = iwafer + 1;
	  
	  index_hyb = svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID);
	  
	  if (index_hyb < 0) continue;
	  
	  fSvtMonitor->setHybridID(hybridID);
	  fSvtMonitor->setWaferID(waferID);

	  if (TString(className) == "StSvtHistAnalog")
	    fSvtMonitor->fillHist();
	  else if (!(((StSvtHybridHist*)svtHist->at(index_hyb))->getHist()->GetEntries())) {
	    if (histID < 200)
	      fSvtMonitor->fillHist();
	    if (histID > 300)
	      fSvtMonitor->fillHistPed();
	  }
	}
      }
    }

    config = TString(svtHist->getConfiguration());
    if (config == "LADDER") {
      barrel = 0;
      ladder = 0;
    }
    else {
      barrel = barrelID;
      ladder = ladderID;
    }
   
    if ( !strncmp(className, "StSvtHist", strlen("StSvtHist")) ) {
      maxADC = svtHist->getMaximum(barrel,ladder);
      minADC = svtHist->getMinimum(barrel,ladder);
    }
    else if ( !strncmp(className, "StSvtHist2D", strlen("StSvtHist2D")) ) {
      maxADC = ((StSvtHist2D*)svtHist)->getMaximum(barrel,ladder);
      minADC = ((StSvtHist2D*)svtHist)->getMinimum(barrel,ladder);
    }
    else if ( !strncmp(className, "StSvtHistAnalog", strlen("StSvtHistAnalog")) ) {
      maxADC = ((StSvtHistAnalog*)svtHist)->getMaximum(barrel,ladder);
      minADC = ((StSvtHistAnalog*)svtHist)->getMinimum(barrel,ladder);
    }
    else if ( !strncmp(className, "StSvtGraph", strlen("StSvtGraph")) ) {
      maxADC = ((StSvtGraph*)svtHist)->getMaximum(barrel,ladder);
      minADC = ((StSvtGraph*)svtHist)->getMinimum(barrel,ladder);
    }
  }
  else {
    cout << "Not a valid HISTOGRAM!!!" << endl;
    return;
  }
    
  if ((TString(option) != "same") && (TString(option) != "Xsame") && (TString(option) != "Ysame"))
    canvas->Clear();
  
  for (int ihybrid=0;ihybrid < numberOfHybrids;ihybrid++) {
    
    hybridID = ihybrid + 1;
    
    if (ihybrid == 1) gStyle->SetPadBottomMargin(0.01);
    
    for (int iwafer=0;iwafer < numberOfWafers;iwafer++) {
	
      waferID = iwafer + 1;
      
      if (svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID) < 0) continue;
      
      if (waferID == 1) 
	gStyle->SetPadLeftMargin(0.2);
      else
	gStyle->SetPadLeftMargin(0.01);
      
      if (waferID == numberOfWafers) 
	gStyle->SetPadRightMargin(0.2);
      else
	gStyle->SetPadRightMargin(0.01);
      
      canvas->cd();

      ipad = iwafer*2+ihybrid;
      strcpy(title,"padHybrid");
      sprintf(append,"_%d",ipad);
      strcat(title,append);
      
      if ((TString(option) != "same") && (TString(option) != "Xsame") && (TString(option) != "Ysame")) {
	mPadLadder[ipad] = new TPad(title,"pad",xlow[iwafer],ylow[ihybrid],xup[iwafer],yup[ihybrid]);
	mPadLadder[ipad]->Draw();
      }
      mPadLadder[ipad]->cd();
    
      fSvtMonitor->setHybridID(hybridID);
      fSvtMonitor->setWaferID(waferID);
      if ((TString(option) == "X") || (TString(option) == "Xsame")) {
	if ((TString(option) == "Xsame") && (ipad == 0))
	  ++mColorIndex;
	projectX(firstybin,lastybin,option);
      }
      else if ((TString(option) == "Y") || (TString(option) == "Ysame")) {
	if ((TString(option) == "Ysame") && (ipad == 0))
	  ++mColorIndex;
	projectY(firstybin,lastybin,option);
      }
      else
	drawHist(option,"LADDER");    
      mPadLadder[ipad]->Update();
    }
  }
    
  fSvtMonitor->setHybridID(h);
  fSvtMonitor->setWaferID(w);
  
  gStyle->SetPadBottomMargin();
  gStyle->SetPadTopMargin();
  gStyle->SetPadRightMargin();
  gStyle->SetPadLeftMargin();
}

void StSvtGuiMonitor::drawHistBarrel(char* option, TCanvas* canvas)
{
  histograms = fSvtMonitor->getHistograms();
  nEvents = fSvtMonitor->getnEvents();
  int l = fSvtMonitor->getLadderID();
  barrelID = fSvtMonitor->getBarrelID();
  histID = fSvtMonitor->getHistID();

  float xlow[16], xup[16], ylow[16], yup[16];
  int index_hyb, indexHist, numberOfLadders;
  char append[10], title[10];

  StSvtHist* svtHist;

  switch(barrelID) {
    
  case 1:
    xlow[0] = 0.;
    xup[0]  = 0.265;
    xlow[1] = 0.265;
    xup[1]  = 0.50;
    xlow[2] = 0.50;
    xup[2]  = 0.735;
    xlow[3] = 0.735;
    xup[3]  = 1.;
    xlow[4] = 0.;
    xup[4]  = 0.265;
    xlow[5] = 0.265;
    xup[5]  = 0.50;
    xlow[6] = 0.50;
    xup[6]  = 0.735;
    xlow[7] = 0.735;
    xup[7]  = 1.;
    ylow[0] = 0.;
    yup[0]  = 0.55;
    ylow[1] = 0.;
    yup[1]  = 0.55;
    ylow[2] = 0.;
    yup[2]  = 0.55;
    ylow[3] = 0.;
    yup[3]  = 0.55;
    ylow[4] = 0.55;
    yup[4]  = 1.;
    ylow[5] = 0.55;
    yup[5]  = 1.;
    ylow[6] = 0.55;
    yup[6]  = 1.;
    ylow[7] = 0.55;
    yup[7]  = 1.;
    numberOfLadders = 8;
    break;

  case 2:
    xlow[0] = 0.;
    xup[0]  = 0.2;
    xlow[1] = 0.2;
    xup[1]  = 0.36;
    xlow[2] = 0.36;
    xup[2]  = 0.52;
    xlow[3] = 0.52;
    xup[3]  = 0.68;
    xlow[4] = 0.68;
    xup[4]  = 0.84;
    xlow[5] = 0.84;
    xup[5]  = 1.;
    xlow[6] = 0.;
    xup[6]  = 0.2;
    xlow[7] = 0.2;
    xup[7]  = 0.36;
    xlow[8] = 0.36;
    xup[8]  = 0.52;
    xlow[9] = 0.52;
    xup[9]  = 0.68;
    xlow[10] = 0.68;
    xup[10]  = 0.84;
    xlow[11] = 0.84;
    xup[11]  = 1.;
    ylow[0] = 0.;
    yup[0]  = 0.55;
    ylow[1] = 0.;
    yup[1]  = 0.55;
    ylow[2] = 0.;
    yup[2]  = 0.55;
    ylow[3] = 0.;
    yup[3]  = 0.55;
    ylow[4] = 0.;
    yup[4]  = 0.55;
    ylow[5] = 0.;
    yup[5]  = 0.55;
    ylow[6] = 0.55;
    yup[6]  = 1.;
    ylow[7] = 0.55;
    yup[7]  = 1.;
    ylow[8] = 0.55;
    yup[8]  = 1.;
    ylow[9] = 0.55;
    yup[9]  = 1.;
    ylow[10] = 0.55;
    yup[10]  = 1.;
    ylow[11] = 0.55;
    yup[11]  = 1.;
    numberOfLadders = 12;
    break;

  case 3:
    xlow[0] = 0.;
    xup[0]  = 0.134;
    xlow[1] = 0.134;
    xup[1]  = 0.256;
    xlow[2] = 0.256;
    xup[2]  = 0.378;
    xlow[3] = 0.378;
    xup[3]  = 0.5;
    xlow[4] = 0.5;
    xup[4]  = 0.622;
    xlow[5] = 0.622;
    xup[5]  = 0.744;
    xlow[6] = 0.744;
    xup[6]  = 0.866;
    xlow[7] = 0.866;
    xup[7]  = 1.;
    xlow[8] = 0.;
    xup[8]  = 0.134;
    xlow[9] = 0.134;
    xup[9]  = 0.256;
    xlow[10] = 0.256;
    xup[10]  = 0.378;
    xlow[11] = 0.378;
    xup[11]  = 0.5;
    xlow[12] = 0.5;
    xup[12]  = 0.622;
    xlow[13] = 0.622;
    xup[13]  = 0.744;
    xlow[14] = 0.744;
    xup[14]  = 0.866;
    xlow[15] = 0.866;
    xup[15]  = 1.;
    ylow[0] = 0.;
    yup[0]  = 0.55;
    ylow[1] = 0.;
    yup[1]  = 0.55;
    ylow[2] = 0.;
    yup[2]  = 0.55;
    ylow[3] = 0.;
    yup[3]  = 0.55;
    ylow[4] = 0.;
    yup[4]  = 0.55;
    ylow[5] = 0.;
    yup[5]  = 0.55;
    ylow[6] = 0.;
    yup[6]  = 0.55;
    ylow[7] = 0.;
    yup[7]  = 0.55;
    ylow[8] = 0.55;
    yup[8]  = 1.;
    ylow[9] = 0.55;
    yup[9]  = 1.;
    ylow[10] = 0.55;
    yup[10]  = 1.;
    ylow[11] = 0.55;
    yup[11]  = 1.;
    ylow[12] = 0.55;
    yup[12]  = 1.;
    ylow[13] = 0.55;
    yup[13]  = 1.;
    ylow[14] = 0.55;
    yup[14]  = 1.;
    ylow[15] = 0.55;
    yup[15]  = 1.;
    numberOfLadders = 16;
    break;
  }

  gStyle->SetPadBottomMargin(0.15);
  gStyle->SetPadTopMargin(0.01);
  gStyle->SetPadRightMargin(0.01);
  gStyle->SetPadLeftMargin(0.2);

  maxADC = -999; 
  minADC = 999; 

  indexHist = fSvtMonitor->getHistIndex(histID);

  if (indexHist >= 0) {

    const char* className = histograms->At(indexHist)->ClassName();

    svtHist = (StSvtHist*)histograms->At(indexHist) ;
    if (!svtHist) {
      cout << "Not a valid HISTOGRAM!!!" << endl;
      return;
    }
             
    for (int iladder=0;iladder < numberOfLadders; iladder++) {
      
      ladderID = iladder + 1;

      index_hyb = svtHist->getHybridIndex(barrelID, ladderID, 0, 0);

      if (index_hyb < 0) continue;
	
      fSvtMonitor->setLadderID(ladderID);

      if (!(((StSvtHybridHist*)svtHist->at(index_hyb))->getHist()->GetEntries())) {
	if (histID < 200)
	  fSvtMonitor->fillHist();
	if (histID > 300)
	  fSvtMonitor->fillHistPed();
      }
    }

    if ( !strncmp(className, "StSvtHist", strlen("StSvtHist")) ) {
      maxADC = svtHist->getMaximum(barrelID);
      minADC = svtHist->getMinimum(barrelID);
    }
    else if ( !strncmp(className, "StSvtHist2D", strlen("StSvtHist2D")) ) {
      maxADC = ((StSvtHist2D*)svtHist)->getMaximum(barrelID);
      minADC = ((StSvtHist2D*)svtHist)->getMinimum(barrelID);
    }
    else if ( !strncmp(className, "StSvtHistAnalog", strlen("StSvtHistAnalog")) ) {
      maxADC = ((StSvtHistAnalog*)svtHist)->getMaximum(barrelID);
      minADC = ((StSvtHistAnalog*)svtHist)->getMinimum(barrelID);
    }
    else if ( !strncmp(className, "StSvtGraph", strlen("StSvtGraph")) ) {
      maxADC = ((StSvtGraph*)svtHist)->getMaximum(barrelID);
      minADC = ((StSvtGraph*)svtHist)->getMinimum(barrelID);
    }
  }
  else {
    cout << "Not a valid HISTOGRAM!!!" << endl;
    return;
  }
    
  if (TString(option) != "same")
    canvas->Clear();
  
  for (int iladder=0;iladder < numberOfLadders; iladder++) {
      
    ladderID = iladder + 1;
    if (ladderID > numberOfLadders/2) gStyle->SetPadBottomMargin(0.01);
	      
    if (svtHist->getHybridIndex(barrelID, ladderID, 0, 0) < 0) continue;
      
    if ((ladderID == 1) || (ladderID == (numberOfLadders/2+1))) 
      gStyle->SetPadLeftMargin(0.2);
    else
      gStyle->SetPadLeftMargin(0.01);
      
    if ((ladderID == numberOfLadders) || (ladderID == numberOfLadders/2)) 
      gStyle->SetPadRightMargin(0.2);
    else
      gStyle->SetPadRightMargin(0.01);
    
    canvas->cd();

    strcpy(title,"padLadder");
    sprintf(append,"_%d",ladderID);
    strcat(title,append);

    if (TString(option) != "same") {
      mPadBarrel[iladder] = new TPad(title,"pad",xlow[iladder],ylow[iladder],xup[iladder],yup[iladder]);
      mPadBarrel[iladder]->Draw();
    }
    mPadBarrel[iladder]->cd();
    
    fSvtMonitor->setLadderID(ladderID);
    drawHist(option,"BARREL");    
    mPadBarrel[iladder]->Update();
  }
    
  fSvtMonitor->setLadderID(l);
  
  gStyle->SetPadBottomMargin();
  gStyle->SetPadTopMargin();
  gStyle->SetPadRightMargin();
  gStyle->SetPadLeftMargin();
}

void StSvtGuiMonitor::drawClusters(StSvtHybridCollection* clustersColl)
{
  hybridID = fSvtMonitor->getHybridID();
  waferID = fSvtMonitor->getWaferID();
  ladderID = fSvtMonitor->getLadderID();
  barrelID = fSvtMonitor->getBarrelID();

  TEllipse Ellipse;
  StThreeVector<double>* position;
  StSvtHitData* hitData;

  Ellipse.SetFillStyle(0);
  Ellipse.SetLineWidth(2);

  StSvtAnalysedHybridClusters* hybridClusters = (StSvtAnalysedHybridClusters*)clustersColl->getObject(barrelID,ladderID,waferID,hybridID);

  if (!hybridClusters) {
    cout << "NO clusters found for this hybrid!!" << endl;
    return;
  }

  for( int i=0; i<hybridClusters->numOfHits(); i++){

    position = hybridClusters->WaferPosition();
    hitData = hybridClusters->svtHitData();

    cout << "Anodes=" << position[i].y() << " Time=" << position[i].x() << endl;
    cout << "Anodes Width =" << hitData[i].mom2[1] << " Time Width=" << hitData[i].mom2[0] << endl;
    Ellipse.DrawEllipse(position[i].y(),position[i].x(),2*hitData[i].mom2[1],2*hitData[i].mom2[0],0.,360.,0.);
  }
}


//*****************************************************************************

TH1F* StSvtGuiMonitor::projectX(int firstybin, int lastybin, char* option)
{
  TOrdCollection* histograms = fSvtMonitor->getHistograms();
  int hybridID = fSvtMonitor->getHybridID();
  int waferID = fSvtMonitor->getWaferID();
  int ladderID = fSvtMonitor->getLadderID();
  int barrelID = fSvtMonitor->getBarrelID();
  int histID = fSvtMonitor->getHistID();

  int startLoop, endLoop, first, last;
  float diff, content;
  char* name = "projX";
  char append[50], temp[100];
  TH1F* projX;
  char drawOption[20];

  int index_hyb, indexHist;
  StSvtHist* svtHist;
  StSvtHybridHist* hybridHist;

  if ((TString(option) == "same") || (TString(option) == "Xsame"))
    sprintf(drawOption,"SAME");
  else
    sprintf(drawOption,"H");

  histProj->Delete();
  indexHist = fSvtMonitor->getHistIndex(histID);
  if (indexHist < 0) return 0;

  svtHist = (StSvtHist*)histograms->At(indexHist) ;
  if (svtHist) {
    index_hyb = svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID);
    hybridHist = (StSvtHybridHist*)svtHist->at(index_hyb);
  }
  else {
    cout << "Not a valid HISTOGRAM!!!" << endl;
    return 0;
  }
    
  TH2* hist;

  const char* className = histograms->At(indexHist)->ClassName();

  if ( !strncmp(className, "StSvtHist2D", strlen("StSvtHist2D")) )
    hist = (TH2F*)hybridHist->getHist() ;
  else if ( !strncmp(className, "StSvtHist", strlen("StSvtHist")) ) { 
    cout << "Not a 2D histogram! Cannot project..." << endl;
    return 0;
  }

  //diff = float(lastybin) - float(firstybin) + 1;

  if (firstybin || lastybin) {
    startLoop = firstybin;
    endLoop = firstybin;
  }
  else {
    startLoop = 1;
    endLoop = N_TIMEBINS;
  }

  gStyle->SetOptStat(10000001);
  gStyle->SetTitleW(.75);
  gStyle->SetOptTitle(1);
  gStyle->SetStatH();
  gStyle->SetStatW();

  for (first = startLoop; first <= endLoop; first++) {
  
    if (firstybin || lastybin)
      last = lastybin;
    else
      last = first;

    strcpy(temp,name);
    sprintf(append,"_%d_%d",first-1,last-1);
    strcat(temp,append);
    
    projX = (TH1F*)hist->ProjectionX(temp, first, last);
    histProj->Add(projX);

    //TAxis *xAxis = hist->GetXaxis();
    for (int binx=1;binx<=hist->GetNbinsX();binx++) {
      //for (int binx=xAxis->GetXmin();binx<xAxis->GetXmax();binx++) {

      diff = 1;
      for (int biny=first;biny<last;biny++) {
	if (hist->GetBinContent(binx,biny) > 0)
	  diff++;
      }

      if (diff > 0) {
	content = projX->GetBinContent(binx)/diff;
	projX->SetBinContent(binx,content);
      }
    }

    if (TString(option) == "same")
      ++mColorIndex;
    else if (TString(option) != "Xsame")
      mColorIndex = 1;
    projX->SetLineColor(mColorIndex);

    //if (diff > 0) projX->Scale(1/diff);
    if ((first!=1) && !(firstybin || lastybin))
      projX->DrawCopy("same");
    else {
      projX->SetMaximum(hist->GetMaximum());
      projX->SetMinimum(hist->GetMinimum());
      projX->DrawCopy(drawOption);
    }
  }

   return projX;
}

TH1F* StSvtGuiMonitor::projectY(int firstxbin, int lastxbin, char* option)
{
  TOrdCollection* histograms = fSvtMonitor->getHistograms();
  int hybridID = fSvtMonitor->getHybridID();
  int waferID = fSvtMonitor->getWaferID();
  int ladderID = fSvtMonitor->getLadderID();
  int barrelID = fSvtMonitor->getBarrelID();
  int histID = fSvtMonitor->getHistID();

  int startLoop, endLoop, first, last;
  float diff, content;
  char* name = "projY";
  char append[50], temp[100];
  TH1F* projY;
  char drawOption[20];

  int index_hyb, indexHist;
  StSvtHist* svtHist;
  StSvtHybridHist* hybridHist;

  if ((TString(option) == "same") || (TString(option) == "Ysame"))
    sprintf(drawOption,"SAME");
  else
    sprintf(drawOption,"H");

  histProj->Delete();
  indexHist = fSvtMonitor->getHistIndex(histID);
  if (indexHist < 0) return 0;

  svtHist = (StSvtHist*)histograms->At(indexHist) ;
  if (svtHist) {
    index_hyb = svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID);
    hybridHist = (StSvtHybridHist*)svtHist->at(index_hyb);
  }
  else {
    cout << "Not a valid HISTOGRAM!!!" << endl;
    return 0;
  }
    
  TH2* hist;

  const char* className = histograms->At(indexHist)->ClassName();

  if ( !strncmp(className, "StSvtHist2D", strlen("StSvtHist2D")) )
    hist = (TH2F*)hybridHist->getHist() ;
  else if ( !strncmp(className, "StSvtHist", strlen("StSvtHist")) ) { 
    cout << "Not a 2D histogram! Cannot project..." << endl;
    return 0;
  }

  diff = float(lastxbin) - float(firstxbin) + 1;

  if (firstxbin || lastxbin) {
    startLoop = firstxbin;
    endLoop = firstxbin;
  }
  else {
    startLoop = 1;
    endLoop = N_TIMEBINS;
  }

  gStyle->SetOptStat(10000001);
  gStyle->SetTitleW(.75);
  gStyle->SetOptTitle(1);
  gStyle->SetStatH();
  gStyle->SetStatW();

  for (first = startLoop; first <= endLoop; first++) {
  
    if (firstxbin || lastxbin)
      last = lastxbin;
    else
      last = first;

    strcpy(temp,name);
    sprintf(append,"_%d_%d",first,last);
    strcat(temp,append);

    projY = (TH1F*)hist->ProjectionY(temp, first, last);
    histProj->Add(projY);

    //TAxis *yAxis = hist->GetYaxis();
    for (int biny=1;biny<=hist->GetNbinsY();biny++) {

      diff = 1;
      for (int binx=first;binx<=last;binx++) {
	if (hist->GetBinContent(binx,biny) > 0)
	  diff++;
      }

      if (diff > 0) {
	content = projY->GetBinContent(biny)/diff;
	projY->SetBinContent(biny,content);
      }
    }

    if (TString(option) == "same")
      ++mColorIndex;
    else if (TString(option) != "Ysame")
      mColorIndex = 1;
    projY->SetLineColor(mColorIndex);

    //if (diff > 0) projY->Scale(1/diff);
    if ((first!=1) && !(firstxbin || lastxbin))
      projY->DrawCopy("same");
    else {
      projY->SetMaximum(hist->GetMaximum());
      projY->SetMinimum(hist->GetMinimum());
      projY->DrawCopy(drawOption);
    }
  }

   return projY;
}

