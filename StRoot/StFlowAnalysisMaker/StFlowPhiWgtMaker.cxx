////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowPhiWgtMaker.cxx,v 1.1 2003/05/16 20:44:49 posk Exp $
//
// Authors: Art Poskanzer and Jamie Dunlop, May 2003
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to produce PhiWgt files using StFlowEvent
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowPhiWgtMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "PhysicalConstants.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TOrdCollection.h"
#include "StMessMgr.h"
#include "TText.h"
#define PR(x) cout << "##### FlowPhiWgt: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowPhiWgtMaker)

//-----------------------------------------------------------------------

StFlowPhiWgtMaker::StFlowPhiWgtMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
}

//-----------------------------------------------------------------------

StFlowPhiWgtMaker::~StFlowPhiWgtMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowPhiWgtMaker::Make() {
  // Make histograms

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {     // event selected
    FillParticleHistograms();              // fill particle histograms
  } else {
    gMessMgr->Info("##### FlowPhiWgt: FlowEvent pointer null");
    return kStOK;
  }  
  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowPhiWgtMaker::Init() {

  // Create the files
  TString* fileName;
  for (int n = 1; n < nCens; n++) {
    char countCens[2];
    sprintf(countCens,"%d",n);
    fileName = new TString("flowPhiWgt");
    fileName->Append(*countCens);
    fileName->Append(".root");
    phiWgtFile[n] = new TFile(fileName->Data(), "RECREATE");
    delete fileName;
  }
  
  // Book histograms
  const float phiMin          =    0.;
  const float phiMax          = twopi; 
  TString* histTitle;

  // for each centrality
  for (int n = 1; n < nCens; n++) {
    phiWgtFile[n]->cd();           // each file is a directory

    // for each selection   
    for (int k = 0; k < Flow::nSels; k++) {
      char countSels[2];
      sprintf(countSels,"%d",k+1);
      
      // for each harmonic
      for (int j = 0; j < Flow::nHars; j++) {
	char countHars[2];
	sprintf(countHars,"%d",j+1);
	
	// Phi lab
	// Tpc (FarEast)
	histTitle = new TString("Flow_Phi_FarEast_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiFarEast = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiFarEast->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiFarEast->SetYTitle("Counts");
	delete histTitle;
	
	// Tpc (East)
	histTitle = new TString("Flow_Phi_East_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiEast = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiEast->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiEast->SetYTitle("Counts");
	delete histTitle;
	
	// Tpc (West)
	histTitle = new TString("Flow_Phi_West_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWest = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWest->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWest->SetYTitle("Counts");
	delete histTitle;
	
	// Tpc (FarWest)
	histTitle = new TString("Flow_Phi_FarWest_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiFarWest = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiFarWest->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiFarWest->SetYTitle("Counts");
	delete histTitle;
	
	// Ftpc (FarEast)
	histTitle = new TString("Flow_Phi_FtpcFarEast_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiFtpcFarEast = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiFtpcFarEast->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiFtpcFarEast->SetYTitle("Counts");
	delete histTitle;
	
	// Ftpc (East)
	histTitle = new TString("Flow_Phi_FtpcEast_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiFtpcEast = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiFtpcEast->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiFtpcEast->SetYTitle("Counts");
	delete histTitle;
	
	// Ftpc (West)
	histTitle = new TString("Flow_Phi_FtpcWest_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiFtpcWest = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiFtpcWest->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiFtpcWest->SetYTitle("Counts");
	delete histTitle;
	
	// Ftpc (FarWest)
	histTitle = new TString("Flow_Phi_FtpcFarWest_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiFtpcFarWest = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiFtpcFarWest->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiFtpcFarWest->SetYTitle("Counts");
	delete histTitle;
	
	
	// PhiWgt new
	// Tpc (FarEast)
	histTitle = new TString("Flow_Phi_Weight_FarEast_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarEast = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarEast->Sumw2();
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarEast->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarEast->SetYTitle("PhiWgt");
	delete histTitle;
	
	// Tpc (East)
	histTitle = new TString("Flow_Phi_Weight_East_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWgtEast = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWgtEast->Sumw2();
	hist[k].histCen[n].histHar[j].mHistPhiWgtEast->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWgtEast->SetYTitle("PhiWgt");
	delete histTitle;
	
	// Tpc (West)
	histTitle = new TString("Flow_Phi_Weight_West_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWgtWest = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWgtWest->Sumw2();
	hist[k].histCen[n].histHar[j].mHistPhiWgtWest->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWgtWest->SetYTitle("PhiWgt");
	delete histTitle;
	
	// Tpc (FarWest)
	histTitle = new TString("Flow_Phi_Weight_FarWest_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarWest = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarWest->Sumw2();
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarWest->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarWest->SetYTitle("PhiWgt");
	delete histTitle;
	
	// Ftpc (FarEast)
	histTitle = new TString("Flow_Phi_Weight_FtpcFarEast_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarEast = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarEast->Sumw2();
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarEast->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarEast->SetYTitle("PhiWgt");
	delete histTitle;
	
	// Ftpc (East)
	histTitle = new TString("Flow_Phi_Weight_FtpcEast_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcEast = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcEast->Sumw2();
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcEast->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcEast->SetYTitle("PhiWgt");
	delete histTitle;
	
	// Ftpc (West)
	histTitle = new TString("Flow_Phi_Weight_FtpcWest_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcWest = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcWest->Sumw2();
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcWest->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcWest->SetYTitle("PhiWgt");
	delete histTitle;
	
	// Ftpc (FarWest)
	histTitle = new TString("Flow_Phi_Weight_FtpcFarWest_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarWest = 
	  new TH1D(histTitle->Data(), histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarWest->Sumw2();
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarWest->SetXTitle
	  ("Azimuthal Angles (rad)");
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarWest->SetYTitle("PhiWgt");
	delete histTitle;
	
      }
    }
  }

  gMessMgr->SetLimit("##### FlowPhiWgt", 2);
  gMessMgr->Info("##### FlowPhiWgt: $Id: StFlowPhiWgtMaker.cxx,v 1.1 2003/05/16 20:44:49 posk Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowPhiWgtMaker::FillParticleHistograms() {
  // Fill histograms from the particles

  // Centrality and vertex of this event
  int iCen = pFlowEvent->Centrality();
  if (!iCen) { return; }  // skip centrality = 0
  Float_t vertexZ = pFlowEvent->VertexPos().z();

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;

    float phi         = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float eta         = pFlowTrack->Eta();
    float pt          = pFlowTrack->Pt();
    float zFirstPoint;
    float zLastPoint;
    if (pFlowEvent->FirstLastPoints()) {
      zFirstPoint = pFlowTrack->ZFirstPoint();
      zLastPoint  = pFlowTrack->ZLastPoint();
    }
    StTrackTopologyMap map = pFlowTrack->TopologyMap();

    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	bool oddHar = (j+1) % 2;
	pFlowSelect->SetHarmonic(j);
	if (pFlowSelect->Select(pFlowTrack)) {
	  // Get detID
	  StDetectorId detId;
	  Bool_t kTpcFarEast  = kFALSE;
	  Bool_t kTpcEast     = kFALSE;
	  Bool_t kTpcWest     = kFALSE;
	  Bool_t kTpcFarWest  = kFALSE;
	  Bool_t kFtpcFarEast = kFALSE;
	  Bool_t kFtpcEast    = kFALSE;
	  Bool_t kFtpcWest    = kFALSE;
	  Bool_t kFtpcFarWest = kFALSE;
	  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) {
	    // Tpc track, or TopologyMap not available
	    detId = kTpcId;
	    // Set TpcEast and West
	    if (pFlowEvent->FirstLastPoints()) {
	      if (zFirstPoint > 0. && zLastPoint > 0.) {
		kTpcFarWest = kTRUE;
	      } else if (zFirstPoint > 0. && zLastPoint < 0.) {
		kTpcWest = kTRUE;
	      } else if (zFirstPoint < 0. && zLastPoint > 0.) {
		kTpcEast = kTRUE;
	      } else {
		kTpcFarEast = kTRUE;
	      }
	    } else {
	      if (eta > 0. && vertexZ > 0.) {
		kTpcFarWest = kTRUE;
	      } else if (eta > 0. && vertexZ < 0.) {
		kTpcWest = kTRUE;
	      } else if (eta < 0. && vertexZ > 0.) {
		kTpcEast = kTRUE;
	      } else {
		kTpcFarEast = kTRUE;
	      }
	    }
	  } else if (map.trackFtpcEast()) {
	    detId = kFtpcEastId;  // eta < 0.
	    if (vertexZ > 0.) {
	      kFtpcEast = kTRUE;
	    } else { // vertexZ < 0.
	      kFtpcFarEast = kTRUE;
	    }
	  } else if (map.trackFtpcWest()) {
	    detId = kFtpcWestId; // eta > 0.
	    if (vertexZ > 0.) {
	      kFtpcFarWest = kTRUE;
	    } else { // vertexZ < 0.
	      kFtpcWest = kTRUE;
	    }
	  } else {
	    detId = kUnknownId;
	  }

	  // Calculate weights for filling histograms
	  float wt = 1.;
	  if (pFlowEvent->PtWgt()) {
	    wt *= (pt < 2.) ? pt : 2.;  // pt weighting going constant above 2 GeV
	  }
	  float etaAbs = fabs(eta);
 	  if (pFlowEvent->EtaWgt() && oddHar && etaAbs > 1.) {
	    wt *= etaAbs;
	  }

	  // Fill histograms
	  if (kFtpcFarEast) {
	    hist[k].histCen[iCen].histHar[j].mHistPhiFtpcFarEast->Fill(phi,wt);
	  } else if (kFtpcEast) {
	    hist[k].histCen[iCen].histHar[j].mHistPhiFtpcEast->Fill(phi,wt);
	  } else if (kFtpcWest) {
	    hist[k].histCen[iCen].histHar[j].mHistPhiFtpcWest->Fill(phi,wt);
	  } else if (kFtpcFarWest) {
	    hist[k].histCen[iCen].histHar[j].mHistPhiFtpcFarWest->Fill(phi,wt);
	  } else if (kTpcFarEast){
	    hist[k].histCen[iCen].histHar[j].mHistPhiFarEast->Fill(phi,wt);
	  } else if (kTpcEast){
	    hist[k].histCen[iCen].histHar[j].mHistPhiEast->Fill(phi,wt);
	  } else if (kTpcWest){
	    hist[k].histCen[iCen].histHar[j].mHistPhiWest->Fill(phi,wt);
	  } else if (kTpcFarWest){
	    hist[k].histCen[iCen].histHar[j].mHistPhiFarWest->Fill(phi,wt);
	  }

	}
      }
    }  
  }

}

//-----------------------------------------------------------------------

Int_t StFlowPhiWgtMaker::Finish() {
  // Outputs phiWgt values

  // PhiWgt histogram collections
  TOrdCollection* phiWgtHistNames[nCens];
  for (int n = 1; n < nCens; n++) {
    phiWgtHistNames[n] = new TOrdCollection(Flow::nSels*Flow::nHars);
  }

  cout << endl << "##### PhiWgt Maker:" << endl;

  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      for (int n = 1; n < nCens; n++) {
	// Calculate PhiWgt
	double meanFarEast = hist[k].histCen[n].histHar[j].mHistPhiFarEast->
	  Integral() / (double)Flow::nPhiBins;
	double meanEast = hist[k].histCen[n].histHar[j].mHistPhiEast->
	  Integral() / (double)Flow::nPhiBins;
	double meanWest = hist[k].histCen[n].histHar[j].mHistPhiWest->
	  Integral() / (double)Flow::nPhiBins;
	double meanFarWest = hist[k].histCen[n].histHar[j].mHistPhiFarWest->
	  Integral() / (double)Flow::nPhiBins;
	double meanFtpcFarEast = hist[k].histCen[n].histHar[j].mHistPhiFtpcFarEast->
	  Integral() / (double)Flow::nPhiBinsFtpc;
	double meanFtpcEast = hist[k].histCen[n].histHar[j].mHistPhiFtpcEast->
	  Integral() / (double)Flow::nPhiBinsFtpc;
	double meanFtpcWest = hist[k].histCen[n].histHar[j].mHistPhiFtpcWest->
	  Integral() / (double)Flow::nPhiBinsFtpc;
	double meanFtpcFarWest = hist[k].histCen[n].histHar[j].mHistPhiFtpcFarWest->
	  Integral() / (double)Flow::nPhiBinsFtpc;
	
	// Tpc
	for (int i = 0; i < Flow::nPhiBins; i++) {
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFarEast->
	    SetBinContent(i+1,meanFarEast);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFarEast->
	    SetBinError(i+1, 0.);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtEast->
	    SetBinContent(i+1, meanEast);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtEast->
	    SetBinError(i+1, 0.);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtWest->
	    SetBinContent(i+1, meanWest);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtWest->
	    SetBinError(i+1, 0.);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFarWest->
	    SetBinContent(i+1,meanFarWest);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFarWest->
	    SetBinError(i+1, 0.);
	}
	
	// Ftpc
	for (int i = 0; i < Flow::nPhiBinsFtpc; i++) {
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarEast->
	    SetBinContent(i+1, meanFtpcFarEast);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarEast->
	    SetBinError(i+1, 0.);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcEast->
	    SetBinContent(i+1, meanFtpcEast);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcEast->
	    SetBinError(i+1, 0.);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcWest->
	    SetBinContent(i+1, meanFtpcWest);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcWest->
	    SetBinError(i+1, 0.);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarWest->
	    SetBinContent(i+1, meanFtpcFarWest);
	  hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarWest->
	    SetBinError(i+1, 0.);
	}
	
	// Tpc
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarEast->
	  Divide(hist[k].histCen[n].histHar[j].mHistPhiFarEast);
	phiWgtHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistPhiWgtFarEast);
	hist[k].histCen[n].histHar[j].mHistPhiWgtEast->
	  Divide(hist[k].histCen[n].histHar[j].mHistPhiEast);
	phiWgtHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistPhiWgtEast);
	hist[k].histCen[n].histHar[j].mHistPhiWgtWest->
	  Divide(hist[k].histCen[n].histHar[j].mHistPhiWest);
	phiWgtHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistPhiWgtWest);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFarWest->
	  Divide(hist[k].histCen[n].histHar[j].mHistPhiFarWest);
	phiWgtHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistPhiWgtFarWest);
	
	// Ftpc
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarEast->
	  Divide(hist[k].histCen[n].histHar[j].mHistPhiFtpcFarEast);
	phiWgtHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarEast);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcEast->
	  Divide(hist[k].histCen[n].histHar[j].mHistPhiFtpcEast);
	phiWgtHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcEast);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcWest->
	  Divide(hist[k].histCen[n].histHar[j].mHistPhiFtpcWest);
	phiWgtHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcWest);
	hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarWest->
	  Divide(hist[k].histCen[n].histHar[j].mHistPhiFtpcFarWest);
	phiWgtHistNames[n]->AddLast(hist[k].histCen[n].histHar[j].mHistPhiWgtFtpcFarWest);
      }
    }
  }
  //GetHistList()->ls();
  
  // Make text object
  TText* textInfo;
  if (pFlowEvent->FirstLastPoints()) {
    char chInfo[400];
    sprintf(chInfo, "%s%d%s%d%s", " pt weight= ", pFlowEvent->PtWgt(),
	    ", eta weight= ", pFlowEvent->EtaWgt(), "\n");
    textInfo = new TText(0,0,chInfo);
  }

  // Write PhiWgt histograms
  for (int n = 1; n < nCens; n++) {
    phiWgtFile[n]->cd();
    if (pFlowEvent->FirstLastPoints()) { textInfo->Write("info"); }
    phiWgtHistNames[n]->Write();
    phiWgtFile[n]->Close();
    delete phiWgtHistNames[n];
  }

  if (pFlowEvent->FirstLastPoints()) delete textInfo;

  delete pFlowSelect;

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPhiWgtMaker.cxx,v $
// Revision 1.1  2003/05/16 20:44:49  posk
// First commit of StFlowPhiWgtMaker
//
//  
////////////////////////////////////////////////////////////////////////////
