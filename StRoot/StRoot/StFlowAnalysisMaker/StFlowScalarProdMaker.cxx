////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowScalarProdMaker.cxx,v 1.12 2004/12/09 23:47:10 posk Exp $
//
// Authors: Method proposed by Art and Sergei, code written by Aihong
//          Frame adopted from Art and Raimond's StFlowAnalysisMaker.
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the scalar product method
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include "StMaker.h"
#include "StFlowScalarProdMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "StEnumerations.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "StMessMgr.h"
#include "TMath.h"
#define PR(x) cout << "##### FlowScalarProdAnalysis: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowScalarProdMaker)

//-----------------------------------------------------------------------

StFlowScalarProdMaker::StFlowScalarProdMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
  SetHistoRanges();
}

StFlowScalarProdMaker::StFlowScalarProdMaker(const Char_t* name,
    const StFlowSelection& flowSelect) : StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
  SetHistoRanges();
}

//-----------------------------------------------------------------------

StFlowScalarProdMaker::~StFlowScalarProdMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowScalarProdMaker::Make() {
  // Fill histograms

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent) {
    if (pFlowSelect->Select(pFlowEvent)) {      // event selected
      FillFromFlowEvent();                      // get event quantities
      FillEventHistograms();                    // fill from FlowEvent
      if (pFlowEvent) FillParticleHistograms(); // fill particle histograms
    }
    if (Debug()) StMaker::PrintInfo();
  } else {
    gMessMgr->Info("##### FlowScalarProdMaker: FlowEvent pointer null");
  }
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowScalarProdMaker::Init() {
  // Book histograms

  float ptMaxPart = Flow::ptMaxPart;
  if (pFlowSelect->PtMaxPart()) {
    ptMaxPart = pFlowSelect->PtMaxPart();
  }
  int nPtBinsPart = Flow::nPtBinsPart;
  if (pFlowSelect->PtBinsPart()) {
    nPtBinsPart = pFlowSelect->PtBinsPart();
  }
  xLabel = "Pseudorapidity";
  if (strlen(pFlowSelect->PidPart()) != 0) { xLabel = "Rapidity"; }

  TString* histTitle;

  // for each selection
  for (int k = 0; k < Flow::nSels; k++) {

    // resolution
    histTitle = new TString("Flow_Res_ScalarProd_Sel");
    *histTitle += k+1;
    histFull[k].mHistRes = new TProfile(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -1.*FLT_MAX, FLT_MAX, "");
    histFull[k].mHistRes->SetXTitle("Harmonic");
    histFull[k].mHistRes->SetYTitle("Resolution");
    delete histTitle;

    // vObs
    histTitle = new TString("Flow_vObs_ScalarProd_Sel");
    *histTitle += k+1;
    histFull[k].mHist_vObs = new TProfile(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -10000., 10000., "");
    histFull[k].mHist_vObs->SetXTitle("Harmonic");
    histFull[k].mHist_vObs->SetYTitle("vObs (%)");
    delete histTitle;
    
    // for each harmonic
    for (int j = 0; j < Flow::nHars; j++) {

      // Flow observed
      histTitle = new TString("Flow_vObs2D_ScalarProd_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vObs2D =	new TProfile2D(histTitle->Data(),
        histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax, nPtBinsPart, 
		 Flow::ptMin, ptMaxPart, -10000., 10000., "");
      histFull[k].histFullHar[j].mHist_vObs2D->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vObs2D->SetYTitle("Pt (GeV/c)");
      delete histTitle;

      // Flow observed profiles
      histTitle = new TString("Flow_vObsEta_ScalarProd_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vObsEta = new TProfile(histTitle->Data(),
        histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax, 
							      -10000., 10000., "");
      histFull[k].histFullHar[j].mHist_vObsEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vObsEta->SetYTitle("v (%)");
      delete histTitle;

      histTitle = new TString("Flow_vObsPt_ScalarProd_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vObsPt = new TProfile(histTitle->Data(),
        histTitle->Data(), nPtBinsPart, Flow::ptMin, ptMaxPart, -10000., 10000., "");
      histFull[k].histFullHar[j].mHist_vObsPt->SetXTitle("Pt (GeV/c)");
      histFull[k].histFullHar[j].mHist_vObsPt->SetYTitle("v (%)");
      delete histTitle;

    }
  }

  gMessMgr->SetLimit("##### FlowScalarProd", 2);
  gMessMgr->Info("##### FlowScalarProdAnalysis: $Id: StFlowScalarProdMaker.cxx,v 1.12 2004/12/09 23:47:10 posk Exp $");

  return StMaker::Init();
}



//-----------------------------------------------------------------------

void StFlowScalarProdMaker::FillFromFlowEvent() {
  // Get Q vectors from StFlowEvent

  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      pFlowSelect->SetHarmonic(j);
      for (int n = 0; n < Flow::nSubs; n++) {
	pFlowSelect->SetSubevent(n);
	int i = Flow::nSels*k + n;
	// sub-event quantities
        mQSub[i][j]=pFlowEvent->Q(pFlowSelect);
      }
      
      pFlowSelect->SetSubevent(-1);
      // full event quantities
      mQ[k][j]    = pFlowEvent->Q(pFlowSelect);
    }
  }

}

//-----------------------------------------------------------------------

void StFlowScalarProdMaker::FillEventHistograms() {
  // The scaler product of the subevent Q vectors

  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      float order  = (float)(j+1);

      histFull[k].mHistRes->Fill(order, (mQSub[Flow::nSels*k + 0][j].X()) * 
	  (mQSub[Flow::nSels*k + 1][j].X()) + (mQSub[Flow::nSels*k + 0][j].Y()) 
				 * (mQSub[Flow::nSels*k + 1][j].Y()) ); 

    }
  }

}

//-----------------------------------------------------------------------

void StFlowScalarProdMaker::FillParticleHistograms() {
  // Fill histograms from the particles

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;

    float phi = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float eta = pFlowTrack->Eta();
    float pt  = pFlowTrack->Pt();
    TVector2 q_i;
    TVector2 u_i;

    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	pFlowSelect->SetHarmonic(j);

	if (pFlowSelect->SelectPart(pFlowTrack)) {
	  bool oddHar = (j+1) % 2;
	  double order  = (double)(j+1);
	  TVector2 mQ_i = mQ[k][j];
	  
	  // Remove autocorrelations
	  if (pFlowSelect->Select(pFlowTrack)) {
	    double phiWgt = pFlowEvent->PhiWeight(k, j, pFlowTrack);
	    q_i.Set(phiWgt * cos(phi * order), phiWgt * sin(phi * order));
	    mQ_i = mQ_i - q_i;
	  }
	  	  
	  // Caculate v for all particles selected
          u_i.Set(cos(phi*order), sin(phi*order));
	  float v = (mQ_i.X()*u_i.X() + mQ_i.Y()*u_i.Y()) /perCent;
	  float vFlip = v;
	  if (eta < 0 && oddHar) vFlip *= -1;
	  if (strlen(pFlowSelect->PidPart()) != 0) { 
	    float rapidity = pFlowTrack->Y();
	    histFull[k].histFullHar[j].mHist_vObs2D-> Fill(rapidity, pt, v);
	    histFull[k].histFullHar[j].mHist_vObsEta->Fill(rapidity, v);
	  } else {
	    histFull[k].histFullHar[j].mHist_vObs2D-> Fill(eta, pt, v);
	    histFull[k].histFullHar[j].mHist_vObsEta->Fill(eta, v);
	  }
	  histFull[k].histFullHar[j].mHist_vObsPt-> Fill(pt, vFlip);
	  histFull[k].mHist_vObs->Fill(order, vFlip);
	  
	}
      }
    }  
  }

}


//-----------------------------------------------------------------------

Int_t StFlowScalarProdMaker::Finish() {
  // Calculates resolution and mean flow values

  TString* histTitle;

  double content;
  double error;
  double totalError;

  cout << endl << "##### Scalar Product Maker:" << endl;

  for (int k = 0; k < Flow::nSels; k++) {

    // Create the 1D v histogram
    histTitle = new TString("Flow_v_ScalarProd_Sel");
    *histTitle += k+1;
    histFull[k].mHist_v = 
      histFull[k].mHist_vObs->ProjectionX(histTitle->Data());
    histFull[k].mHist_v->SetTitle(histTitle->Data());
    histFull[k].mHist_v->SetXTitle("Harmonic");
    histFull[k].mHist_v->SetYTitle("v (%)");
    delete histTitle;
    AddHist(histFull[k].mHist_v);

    for (int j = 0; j < Flow::nHars; j++) {

      //Calculate the resolution
      mRes[k][j]    = ::sqrt(histFull[k].mHistRes->GetBinContent(j+1))*2.;
      mResErr[k][j] = (histFull[k].mHistRes->GetBinError(j+1))*2./mRes[k][j];

      // Create the v 2D histogram
      histTitle = new TString("Flow_v2D_ScalarProd_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_v2D = 
	histFull[k].histFullHar[j].mHist_vObs2D->ProjectionXY(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_v2D->SetYTitle("Pt (GeV/c)");
      histFull[k].histFullHar[j].mHist_v2D->SetZTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_v2D);

      // Create the 1D v histograms
      histTitle = new TString("Flow_vEta_ScalarProd_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vEta = 
	histFull[k].histFullHar[j].mHist_vObsEta->ProjectionX(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vEta->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vEta->SetYTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vEta);

      TString* histTitle = new TString("Flow_vPt_ScalarProd_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vPt = 
	histFull[k].histFullHar[j].mHist_vObsPt->ProjectionX(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vPt->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vPt->SetXTitle("Pt (GeV/c)");
      histFull[k].histFullHar[j].mHist_vPt->SetYTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vPt);

      // Calulate v = vObs / Resolution or Q.u/(2::sqrt(<Q_a.Q_b>))
      if (mRes[k][j]) {
	cout << "##### Resolution of the " << j+1 << "th harmonic = " << 
	  mRes[k][j] << " +/- " << mResErr[k][j] << endl;
	// The systematic error of the resolution is not folded in.
	histFull[k].histFullHar[j].mHist_v2D ->Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vEta->Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vPt ->Scale(1. / mRes[k][j]);
	content = histFull[k].mHist_v->GetBinContent(j+1);
	content /=  mRes[k][j];
	histFull[k].mHist_v->SetBinContent(j+1, content);
	// The systematic error of the resolution is folded in.
	error = histFull[k].mHist_v->GetBinError(j+1);
	error /= mRes[k][j];
	totalError = fabs(content) * ::sqrt((error/content)*(error/content) +
	       (mResErr[k][j]/mRes[k][j])*(mResErr[k][j]/mRes[k][j]));
	histFull[k].mHist_v->SetBinError(j+1, totalError);
	cout << "##### v" << j+1 << "= (" << content << " +/- " << error << 
	  " +/- " << totalError << "(with syst.)) %" << endl;
      } else {
	cout << "##### Resolution of the " << j+1 << "th harmonic was zero."
	     << endl;
	histFull[k].histFullHar[j].mHist_v2D ->Reset();
	histFull[k].histFullHar[j].mHist_vEta->Reset();
	histFull[k].histFullHar[j].mHist_vPt ->Reset();
	histFull[k].mHist_v->SetBinContent(j+1, 0.);
	histFull[k].mHist_v->SetBinError(j+1, 0.);
      }
    }
  }
  //GetHistList()->ls();

  // Write all histograms
  TFile histFile("flow.scalar.root", "RECREATE");
  GetHistList()->Write();
  histFile.Close();
  
  delete pFlowSelect;

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

void StFlowScalarProdMaker::SetHistoRanges(Bool_t ftpc_included) {

    if (ftpc_included) {
	  mEtaMin = Flow::etaMin;
	  mEtaMax = Flow::etaMax;
	mNEtaBins = Flow::nEtaBins;
    } else {
	  mEtaMin = Flow::etaMinTpcOnly;
 	  mEtaMax = Flow::etaMaxTpcOnly;
	mNEtaBins = Flow::nEtaBinsTpcOnly;
    }

    return;
}

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowScalarProdMaker.cxx,v $
// Revision 1.12  2004/12/09 23:47:10  posk
// Minor changes in code formatting.
// Added hist for TPC primary dca to AnalysisMaker.
//
// Revision 1.11  2003/09/02 17:58:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.10  2003/07/07 21:58:20  posk
// Made units of momentum GeV/c instead of GeV.
//
// Revision 1.9  2003/02/24 19:35:27  posk
// Corrected mistake in the error of the resolution.
// This only affected doubly integrated v values.
//
// Revision 1.8  2003/01/10 16:40:49  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.7  2002/05/21 18:42:17  posk
// Kirill's correction to minBias.C for bins with one count.
//
// Revision 1.6  2002/02/18 01:11:53  jeromel
// Mandatory fix for FLT_MAX fix i.e. include float.h
//
// Revision 1.5  2002/02/13 22:31:46  posk
// Pt Weight now also weights Phi Weight. Added Eta Weught, default=FALSE.
//
// Revision 1.4  2002/01/31 01:09:30  posk
// *** empty log message ***
//
// Revision 1.3  2002/01/14 23:42:52  posk
// Renamed ScalerProd histograms. Moved print commands to FlowMaker::Finish().
//
// Revision 1.2  2001/12/21 17:01:59  aihong
// minor changes
//
// Revision 1.1  2001/12/18 23:46:47  aihong
// install scalar product method
//
//  
////////////////////////////////////////////////////////////////////////////
