//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.cxx,v 1.7 2002/11/15 22:23:02 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent 
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.cxx,v $
// Revision 1.7  2002/11/15 22:23:02  posk
// updates.
//
// Revision 1.6  2002/01/16 18:16:57  posk
// Normalized q by sum of weights instead of multiplicity.
//
// Revision 1.5  2001/11/06 17:05:28  posk
// New 40 Gev centrality bins. Using only sin terms at 40 GeV.
//
// Revision 1.4  2001/08/17 22:10:24  posk
// Now also can do 40 GeV data.
//
// Revision 1.3  2001/05/14 23:04:29  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.2  2001/03/16 22:39:23  posk
// Removed pt weighting for odd harmonics.
//
// Revision 1.17  2000/10/12 22:46:35  snelling
//
//////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StFlowEvent.h"
#include "StFlowTrackCollection.h"
#include "StFlowSelection.h"
#include "StFlowConstants.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#define PR(x) cout << "##### FlowEvent: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowEvent)

// Float_t  StFlowEvent::mYCuts[2][Flow::nHars][Flow::nSels] =   {{{1.,4.0},
// 								{1.,2.4},
// 								{1.,4.0},
// 								{1.,2.4},
// 								{1.,4.0},
// 								{1.,2.4}},
// 							       {{6.0,6.},
// 								{6.0,5.},
// 								{6.0,6.},
// 								{6.0,5.},
// 								{6.0,6.},
// 								{6.0,5.}}};

// Float_t  StFlowEvent::mPtCuts[2][Flow::nHars][Flow::nSels] =  {{{0.,0.},
// 								{0.,0.},
// 								{0.,0.},
// 								{0.,0.},
// 								{0.,0.},
// 								{0.,0.}},
// 							       {{1.,1.},
// 								{1.,1.},
// 								{1.,1.},
// 								{1.,1.},
// 								{1.,1.},
// 								{1.,1.}}};

Float_t  StFlowEvent::mYCuts[2][Flow::nHars][Flow::nSels] =   {{{1.,4.0},
								{1.,2.4},
								{1.,4.0}},
							       {{6.0,6.},
								{6.0,5.},
								{6.0,6.}}};

Float_t  StFlowEvent::mPtCuts[2][Flow::nHars][Flow::nSels] =  {{{0.,0.},
								{0.,0.},
								{0.,0.}},
							       {{1.,1.},
								{1.,1.},
								{1.,1.}}};

Float_t  StFlowEvent::mMeanSinCosCuts[2] = {-0.2,0.2};

Bool_t  StFlowEvent::mPtWgt     = kFALSE;
Bool_t  StFlowEvent::mYWgt      = kFALSE;
Bool_t  StFlowEvent::mSinOnly   = kFALSE;
Int_t   StFlowEvent::mStripes   = 0;
Bool_t  StFlowEvent::mProbPid   = kFALSE;
Char_t  StFlowEvent::mPid[10]   = {'\0'};

//-----------------------------------------------------------

StFlowEvent::StFlowEvent()  {
  // Make a new track collection

  pTrackCollection = new StFlowTrackCollection;
 
}

//-----------------------------------------------------------

StFlowEvent::~StFlowEvent() {

  delete pTrackCollection;

}

//-------------------------------------------------------------

Double_t StFlowEvent::PhiWeight(Float_t phi, Int_t selN, Int_t harN) const {

  if (phi < 0.) phi += twopi;
  int n = (int)((phi/twopi)*Flow::nPhiBins);

  return mPhiWgt[selN][harN][n];
}

//-------------------------------------------------------------

Double_t StFlowEvent::MeanCos(Float_t y, Float_t pt, Int_t harN) const {

  if (y > Flow::sinCosYMin && y < Flow::sinCosYMax && pt < Flow::sinCosPtMax) {
      Int_t ptBin = (Int_t)(pt * (float)Flow::nSinCosPtBins/Flow::sinCosPtMax);
      float ySlope = (float)Flow::nSinCosYBins/
	(Flow::sinCosYMax - Flow::sinCosYMin);
      Int_t yBin = (Int_t)((y - Flow::sinCosYMin) * ySlope);
      return  mMeanCos[harN][yBin][ptBin];
  } else {
    return 0.;
  }
}

//-------------------------------------------------------------

Double_t StFlowEvent::MeanSin(Float_t y, Float_t pt, Int_t harN) const {

  if (y > Flow::sinCosYMin && y < Flow::sinCosYMax && pt < Flow::sinCosPtMax) {
      Int_t ptBin = (Int_t)(pt * (float)Flow::nSinCosPtBins/Flow::sinCosPtMax);
      float ySlope = (float)Flow::nSinCosYBins/
	(Flow::sinCosYMax - Flow::sinCosYMin);
      Int_t yBin = (Int_t)((y - Flow::sinCosYMin) * ySlope);
      return  mMeanSin[harN][yBin][ptBin];
  } else {
    return 0.;
  }
}

//-------------------------------------------------------------

UInt_t StFlowEvent::Mult(StFlowSelection* pFlowSelect) {
  UInt_t mult = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack))  mult++;
  }

  return mult;
}

//-------------------------------------------------------------

Float_t StFlowEvent::SumPt(StFlowSelection* pFlowSelect) {
 Float_t sumPt = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      float pt = pFlowTrack->Pt();
      sumPt += pt;
    }
  }

  return sumPt;
}

//-------------------------------------------------------------

Float_t StFlowEvent::SumPt2(StFlowSelection* pFlowSelect) {
 Float_t sumPt2 = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      float pt = pFlowTrack->Pt();
      sumPt2 += pt*pt;
    }
  }

  return sumPt2;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::Q(StFlowSelection* pFlowSelect) { 
  TVector2 mQ;
  Int_t  selN  = pFlowSelect->Sel();
  Int_t  harN  = pFlowSelect->Har();
  bool oddHar  = (harN + 1) % 2;
  double order = (double)(harN + 1);
  double mQx=0., mQy=0.;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      float phi = pFlowTrack->Phi();
      float pt  = pFlowTrack->Pt();
      float y   = pFlowTrack->Y();
      double phiWgt = PhiWeight(phi, selN, harN);
      if (y < Flow::yCM && oddHar) phiWgt *= -1.;
      if (mPtWgt && !oddHar) phiWgt *= pt;
      if (mYWgt && oddHar) phiWgt *= fabs(y - Flow::yCM);
      double meanCos = MeanCos(y, pt, harN);
      double meanSin = MeanSin(y, pt, harN);
      mQx += phiWgt * (cos(phi * order) - meanCos);
      mQy += phiWgt * (sin(phi * order) - meanSin);
    }
  }
  mQ.Set(mQx, mQy);

  return mQ;
}

//-------------------------------------------------------------

Float_t StFlowEvent::Psi(StFlowSelection* pFlowSelect) {
  Int_t  harN = pFlowSelect->Har();
  float order = (float)(harN + 1);
  Float_t psi = 0.;

  TVector2 mQ = Q(pFlowSelect);
  if (mQ.Mod()) {
    psi= mQ.Phi() / order;
    if (psi < 0.) { psi += twopi / order; }
  }
  
  return psi;
}

//-------------------------------------------------------------

Float_t StFlowEvent::q(StFlowSelection* pFlowSelect) {
  //return normalized Q = Q / sqrt(sum of weights**2)

  TVector2 mQ;
  Int_t  selN  = pFlowSelect->Sel();
  Int_t  harN  = pFlowSelect->Har();
  Double_t mQx=0., mQy=0.;
  Double_t SumOfWeightSqr = 0;
  Double_t order = (Double_t)(harN + 1);


  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      Float_t mPhi = pFlowTrack->Phi();
      Double_t phiWgt = PhiWeight(mPhi, selN, harN);
      if (pFlowTrack->Eta() < 0. && (harN+1) % 2 == 1) phiWgt *= -1.;
      if (mPtWgt) {
	Float_t pt = pFlowTrack->Pt();
	phiWgt *= pt;
      }
      SumOfWeightSqr += phiWgt*phiWgt;
      mQx += phiWgt * cos(mPhi * order);
      mQy += phiWgt * sin(mPhi * order);
    }
  }
  
  if (SumOfWeightSqr) {
    mQ.Set(mQx/sqrt(SumOfWeightSqr), mQy/sqrt(SumOfWeightSqr));
  } else {
    mQ.Set(0.,0.);
  }

  return mQ.Mod();
}

//-----------------------------------------------------------------------

void StFlowEvent::SetSelections() {

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    Float_t pt  = pFlowTrack->Pt();
    Float_t rapidity = pFlowTrack->Y();
    Char_t pid[10];
    strcpy(pid, pFlowTrack->Pid());
    for (int selN = 0; selN < Flow::nSels; selN++) {
      for (int harN = 0; harN < Flow::nHars; harN++) {

	// No protons for odd harmonics
 	bool oddHar  = (harN + 1) % 2;
 	if (oddHar && strcmp(pid, "proton") == 0) continue;
	
	// PID
	if (mPid[0] != '\0' && strstr(pid, mPid)==0) continue;
    
	// Pt
	if (mPtCuts[1][harN][selN] > mPtCuts[0][harN][selN] && 
	    (pt < mPtCuts[0][harN][selN] ||
	     pt >= mPtCuts[1][harN][selN])) continue;

	// Rapidity
	if (mYCuts[1][harN][selN] > mYCuts[0][harN][selN] && 
	    (rapidity < mYCuts[0][harN][selN] ||
	     rapidity >= mYCuts[1][harN][selN])) continue;
	
	// MeanSinCos
	Float_t meanSin = MeanSin(rapidity, pt, harN);
	Float_t meanCos = MeanCos(rapidity, pt, harN);
	//for using sin terms only for correlation
	if (harN == 1 && mSinOnly) {
	  if (mMeanSinCosCuts[1] > mMeanSinCosCuts[0] && 
	      (meanSin < mMeanSinCosCuts[0]  ||
	       meanSin >= mMeanSinCosCuts[1])) continue;
	} else {
	  if (mMeanSinCosCuts[1] > mMeanSinCosCuts[0] && 
	      (meanSin < mMeanSinCosCuts[0]  ||
	       meanSin >= mMeanSinCosCuts[1] ||
	       meanCos < mMeanSinCosCuts[0]  ||
	       meanCos >= mMeanSinCosCuts[1])) continue;
	}
	
      	pFlowTrack->SetSelect(harN, selN);
	
      }
    }
  }
  
}

//-------------------------------------------------------------

void StFlowEvent::MakeSubEvents() {

  StFlowTrackIterator itr;
  int eventMult[Flow::nHars][Flow::nSels] = {{0}};
  int harN, selN, subN = 0;
  
  // loop to count the total number of tracks for each selection
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    for (selN = 0; selN < Flow::nSels; selN++) {
      for (harN = 0; harN < Flow::nHars; harN++) {
	if (pFlowTrack->Select(harN, selN)) {
	    eventMult[harN][selN]++;
	}
      }
    }
  }

  // loop to set the SubEvent member variable
  for (selN = 0; selN < Flow::nSels; selN++) {
    for (harN = 0; harN < Flow::nHars; harN++) {
      int subEventMult = eventMult[harN][selN] / Flow::nSubs;
      if (subEventMult) {
	subN = 0;
	int countN = 0;
	for (itr = TrackCollection()->begin(); 
	     itr != TrackCollection()->end(); itr++) {
	  StFlowTrack* pFlowTrack = *itr;
	  if (pFlowTrack->Select(harN, selN)) {
	    pFlowTrack->SetSubevent(harN, selN, subN);
	    countN++;
	    if (countN % subEventMult == 0.) subN++;
	  }
	}
      }
    }
  }
  
}

//-------------------------------------------------------------

void StFlowEvent::MakeStripedSubs() {
  // Subevents of width 0.1 in y.
  // For mStripes = 1, every other odd stipe goes in subevent 0 or 1.
  // For mStripes = 2, it is the even stripes.

  StFlowTrackIterator itr;
  int subN;

  for (int selN = 0; selN < Flow::nSels; selN++) {
    for (int harN = 0; harN < Flow::nHars; harN++) {
      for (itr = TrackCollection()->begin(); 
	   itr != TrackCollection()->end(); itr++) {
	StFlowTrack* pFlowTrack = *itr;
	if (pFlowTrack->Select(harN, selN)) {
	  int stripe = ((int)(pFlowTrack->Y()/0.1)) % 4;
	  if (mStripes == 1) {
	    switch (stripe) {
	    case 0 : subN = 0;
	      break;
	    case 1 : subN = 2;
	      break;
	    case 2 : subN = 1;
	      break;
	    case 3 : subN = 2;
	      break;
	    default: subN = 2;
	    }
	  } else if (mStripes == 2) {
	    switch (stripe) {
	    case 0 : subN = 2;
	      break;
	    case 1 : subN = 1;
	      break;
	    case 2 : subN = 2;
	      break;
	    case 3 : subN = 0;
	      break;
	    default: subN = 2;
	    }
	  }
	  pFlowTrack->SetSubevent(harN, selN, subN);
	}
      }
    }
  }
  
}

//-------------------------------------------------------------

void StFlowEvent::SetPids() {
  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    Float_t p        = pFlowTrack->P();
    Float_t dedx     = pFlowTrack->Dedx();
    Float_t dedxMain = pFlowTrack->DedxMain();
    Short_t charge   = pFlowTrack->Charge();
    double lnp       = log(p);
    double lnp2      = lnp * lnp;
    double prUp = 0.87 + 0.0678*lnp + 0.0176*lnp2;
    double piLo = 0.931 + 0.136*lnp - 0.001 *lnp2;
    double piUp = 1.232 + 0.159*lnp - 0.0152*lnp2;
    if (Flow::eBeam == 158) {
      if (dedxMain > 0.) {
	if (charge == 1 && p > 7.5 && dedx < prUp) {
	  pFlowTrack->SetPid("proton");
	} else if (dedx > piLo && dedx < piUp) {
	  if (charge == 1) { 
	    pFlowTrack->SetPid("pi+");
	  } else if (charge == -1) { 
	    pFlowTrack->SetPid("pi-");
	  }
	}
      } else {
	if ((dedx > piLo || p < 3. || charge != +1) && dedx < piUp) {
	  if (charge == 1) {
	    pFlowTrack->SetPid("pi+");
	  } else if (charge == -1) {
	    pFlowTrack->SetPid("pi-");
	  }
	}
      }
    } else if (Flow::eBeam == 40) {
    double prUp = 0.8913 + 0.089*lnp + 0.0103*lnp2;
      if (dedxMain > 0.) {
 	if (charge == 1 && lnp > 1.2 && dedx < prUp) { 
 	  pFlowTrack->SetPid("proton");
 	} else if (dedx > piLo && dedx < piUp) {
 	  if (charge == 1) { 
 	    pFlowTrack->SetPid("pi+");
 	  } else if (charge == -1) { 
 	    pFlowTrack->SetPid("pi-");
 	  }
 	}
      } else {
 	if ((dedx > piLo || p < 3. || charge != +1) && dedx < piUp) {
 	  if (charge == 1) {
 	    pFlowTrack->SetPid("pi+");
 	  } else if (charge == -1) {
 	    pFlowTrack->SetPid("pi-");
 	  }
	}
      }
    }
    if (strlen(pFlowTrack->Pid()) == 0) pFlowTrack->SetPid("none");
  }

}

//-------------------------------------------------------------

void StFlowEvent::SetRapidities() {
  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    Float_t p   = pFlowTrack->P();
    Float_t pt  = pFlowTrack->Pt();
    Float_t eta = pFlowTrack->Eta();
    
    const Char_t* pid = pFlowTrack->Pid();
    float M = 0.139; 
    if (strcmp(pid, "none") == 0)          { M = 0.139; }
    else if (strcmp(pid, "pi+") == 0)      { M = 0.139; }
    else if (strcmp(pid, "pi-") == 0)      { M = 0.139; }
    else if (strcmp(pid, "proton") == 0)   { M = 0.938; }
    else if (strcmp(pid, "pbar") == 0)     { M = 0.938; }
    else if (strcmp(pid, "e-") == 0)       { M = 0.0005; }
    else if (strcmp(pid, "e+") == 0)       { M = 0.0005; }
    double Pz = sqrt(p*p - pt*pt); 
    if (eta < 0) { Pz = -Pz; }
    double E = sqrt(p*p + M*M);
    Float_t rapidity = 0.5*log((E + Pz)/(E - Pz));
    pFlowTrack->SetY(rapidity);
  }

}

//-----------------------------------------------------------------------

void StFlowEvent::PrintSelectionList() {
  // Prints the list of selection cuts
  // Call in Finish

  cout << "#######################################################" << endl;
  cout << "# Weighting and Striping:" << endl; 
  if (mPtWgt) {
    cout << "#    PtWgt= TRUE" << endl;
  } else {
    cout << "#    PtWgt= FALSE" << endl;
  }
  if (mYWgt) {
    cout << "#    YWgt= TRUE" << endl;
  } else {
    cout << "#    YWgt= FALSE" << endl;
  }
  cout << "#    Stripes= " << mStripes << endl;
  cout << "#######################################################" << endl;
  cout << "# Track Selections List:" << endl; 
  cout << "#    Particles used for the event plane: " << mPid << endl;
  cout << "#    MeanSinCos cuts= " << mMeanSinCosCuts[0] << ", " 
	   << mMeanSinCosCuts[1] << endl;
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      cout << "#  selection= " << k+1 << " harmonic= " 
	   << j+1 << endl;
      cout << "#    Pt cuts= " << mPtCuts[0][j][k] << ", "
	   << mPtCuts[1][j][k] << endl;
      cout << "#    Y cuts= " << mYCuts[0][j][k] << ", " 
	   << mYCuts[1][j][k] << endl;
    }
  }
  cout << "#######################################################" << endl;
  
}
