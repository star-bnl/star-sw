////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowReCentMaker.cxx,v 1.1 2009/11/24 19:29:15 posk Exp $
//
// Authors: Art Poskanzer, Sep 2009
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to produce ReCent files for each harmonic
//                 using StFlowEvent
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowReCentMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "PhysicalConstants.h"
#include "TFile.h"
#include "TString.h"
#include "TProfile.h"
#include "TOrdCollection.h"
#include "StMessMgr.h"
#define PR(x) cout << "##### FlowReCent: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowReCentMaker)

//-----------------------------------------------------------------------

StFlowReCentMaker::StFlowReCentMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
}

//-----------------------------------------------------------------------

StFlowReCentMaker::~StFlowReCentMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowReCentMaker::Make() {
  // Make histograms

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {     // event selected
    FillEventHistograms();              // fill particle histograms
  } else {
    gMessMgr->Info("##### FlowReCent: FlowEvent pointer null");
    return kStOK;
  }  
  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowReCentMaker::Init() {

  // Create the files
  TString* fileName;
  for (int n = 1; n < nCens; n++) {
    fileName = new TString("flowReCent");
    *fileName += n;
    fileName->Append(".root");
    reCentFile[n] = new TFile(fileName->Data(), "RECREATE");
    delete fileName;
  }
  
  // Book histograms
  TString* histTitle;

  // for each centrality
  for (int n = 1; n < nCens; n++) {
    reCentFile[n]->cd();           // each file is a directory

    // for each selection   
    for (int k = 0; k < Flow::nSels; k++) {
      
      // for each harmonic
      for (int j = 0; j < Flow::nHars; j++) {
	
	// Recenter, for exporting
	histTitle = new TString("FlowReCentX_Sel");
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	hist[k].histCen[n].histHar[j].mHistReCentX = new TProfile(histTitle->Data(),
							       histTitle->Data(), 3, 0.5, 3.5);
	hist[k].histCen[n].histHar[j].mHistReCentX->SetXTitle("FTPCE, FTPCW, TPCE, TPCW");
	hist[k].histCen[n].histHar[j].mHistReCentX->SetYTitle("<cos n #phi>");
	delete histTitle;
	
	histTitle = new TString("FlowReCentY_Sel");
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	hist[k].histCen[n].histHar[j].mHistReCentY = new TProfile(histTitle->Data(),
							       histTitle->Data(), 3, 0.5, 3.5);
	hist[k].histCen[n].histHar[j].mHistReCentY->SetXTitle("FTPCE, FTPCW, TPCE, TPCW");
	hist[k].histCen[n].histHar[j].mHistReCentY->SetYTitle("<sin n #phi>");
	delete histTitle;
	
      }
    }
  }

  gMessMgr->SetLimit("##### FlowReCent", 2);
  gMessMgr->Info("##### FlowReCent: $Id: StFlowReCentMaker.cxx,v 1.1 2009/11/24 19:29:15 posk Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowReCentMaker::FillEventHistograms() {
  // Fill histograms

  // Centrality of this event
  int n = pFlowEvent->Centrality();
  if (!n) { return; }  // skip centrality = 0
  
  // calculate recentering parameters, fill 4 bins
  TVector2 qReCent;
  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      pFlowSelect->SetHarmonic(j);
      qReCent = pFlowEvent->ReCentEPPar(pFlowSelect,"FTPCE");
      if (qReCent.X()) hist[k].histCen[n].histHar[j].mHistReCentX->Fill(1., qReCent.X());
      if (qReCent.Y()) hist[k].histCen[n].histHar[j].mHistReCentY->Fill(1., qReCent.Y());
      qReCent = pFlowEvent->ReCentEPPar(pFlowSelect,"FTPCW");
      if (qReCent.X()) hist[k].histCen[n].histHar[j].mHistReCentX->Fill(2., qReCent.X());
      if (qReCent.Y()) hist[k].histCen[n].histHar[j].mHistReCentY->Fill(2., qReCent.Y());
      qReCent = pFlowEvent->ReCentEPPar(pFlowSelect,"TPCE");
      if (qReCent.X()) hist[k].histCen[n].histHar[j].mHistReCentX->Fill(3., qReCent.X());
      if (qReCent.Y()) hist[k].histCen[n].histHar[j].mHistReCentY->Fill(3., qReCent.Y());
      qReCent = pFlowEvent->ReCentEPPar(pFlowSelect,"TPCW");
      if (qReCent.X()) hist[k].histCen[n].histHar[j].mHistReCentX->Fill(4., qReCent.X());
      if (qReCent.Y()) hist[k].histCen[n].histHar[j].mHistReCentY->Fill(4., qReCent.Y());
    }
  }
}

//-----------------------------------------------------------------------

Int_t StFlowReCentMaker::Finish() {
  // Outputs reCent values

  // ReCent histogram collections
  TOrdCollection* reCentHistNames[nCens];
  for (int n = 1; n < nCens; n++) {
    reCentHistNames[n]  = new TOrdCollection(Flow::nSels * Flow::nHars * 2);
  }

  cout << endl << "##### ReCent Maker:" << endl;
  
  // Write out the recentering parameters
  for (int n = 1; n < nCens; n++) {
    for (int k = 0; k < Flow::nSels; k++) {
      for (int j = 0; j < Flow::nHars; j++) {
	reCentHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistReCentX);
	reCentHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistReCentY);
      }
    }
  }

  GetHistList()->ls();
  
  // Write ReCent histograms
  for (int n = 1; n < nCens; n++) {
    reCentFile[n]->cd();
    reCentHistNames[n]->Write();
    reCentFile[n]->Close();
    delete reCentHistNames[n];
  }

  delete pFlowSelect;

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowReCentMaker.cxx,v $
// Revision 1.1  2009/11/24 19:29:15  posk
// Added reCenter to remove acceptance correlations as an option instead of phiWgt.
//
//
//  
////////////////////////////////////////////////////////////////////////////
