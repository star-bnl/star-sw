//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.cxx,v 1.34 2002/06/10 22:50:59 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          Cumulants added by Aihong Tang, KSU, Nov 2001
//
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent 
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
#include "StEnumerations.h"
#include "TVector2.h"
#define PR(x) cout << "##### FlowEvent: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowEvent)

Float_t  StFlowEvent::mEtaTpcCuts[2][Flow::nHars][Flow::nSels] = {{{0.,0.5},
                                                                   {0.,0.},
								   {0.,0.5} },
// 								   {0.,0.},
// 								   {0.,0.5},
// 								   {0.,0.}},
							          {{1.0,2.},
								   {1.0,1.},
							   	   {1.0,2.} }};
// 								   {1.0,1.},
// 								   {1.0,2.},
// 								   {1.0,1.}}};
Float_t  StFlowEvent::mEtaFtpcCuts[2][Flow::nHars][Flow::nSels] = {{{2.7,2.7},
								    {2.7,2.7},
								    {2.7,2.7} },
// 								    {2.7,2.7},
// 								    {2.7,2.7},
// 								    {2.7,2.7}},
							           {{4.0,4.0},
								    {4.0,4.0},
								    {4.0,4.0} }};
// 								    {4.0,4.0},
// 								    {4.0,4.0},
// 								    {4.0,4.0}}};

Float_t  StFlowEvent::mPtTpcCuts[2][Flow::nHars][Flow::nSels] =  {{{0.1,0.1},
								   {0.1,0.1},
								   {0.1,0.1} },
// 								   {0.1,0.1},
// 								   {0.1,0.1},
//   								   {0.1,0.1}},
							          {{2.,2.},
								   {2.,2.},
								   {2.,2.} }};
// 								   {2.,2.},
// 								   {2.,2.},
// 								   {2.,2.}}};
Float_t  StFlowEvent::mPtFtpcCuts[2][Flow::nHars][Flow::nSels] =  {{{0.1,0.1},
								    {0.1,0.1},
								    {0.1,0.1} },
// 								    {0.1,0.1},
// 								    {0.1,0.1},
// 								    {0.1,0.1}},
							           {{2.,2.},
								    {2.,2.},
								    {2.,2.} }};
// 								    {2.,2.},
// 								    {2.,2.},
// 								    {2.,2.}}};

Float_t StFlowEvent::mPiPlusCuts[2]        = {-3., 3.};
Float_t StFlowEvent::mPiMinusCuts[2]       = {-3., 3.};
Float_t StFlowEvent::mProtonCuts[2]        = {-3., 3.};
Float_t StFlowEvent::mAntiProtonCuts[2]    = {-3., 3.};
Float_t StFlowEvent::mKMinusCuts[2]        = {-3., 3.};
Float_t StFlowEvent::mKPlusCuts[2]         = {-3., 3.};
Float_t StFlowEvent::mDeuteronCuts[2]      = {-3., 3.};
Float_t StFlowEvent::mAntiDeuteronCuts[2]  = {-3., 3.};
Float_t StFlowEvent::mElectronCuts[2]      = {-3., 3.};
Float_t StFlowEvent::mPositronCuts[2]      = {-3., 3.};
Float_t StFlowEvent::mDcaGlobalCuts[2]     = { 0., 0.};
Bool_t  StFlowEvent::mPtWgt                = kTRUE;
Bool_t  StFlowEvent::mEtaWgt               = kTRUE;
Bool_t  StFlowEvent::mProbPid              = kFALSE;
Bool_t  StFlowEvent::mEtaSubs              = kFALSE;
Bool_t  StFlowEvent::mOnePhiWgt            = kFALSE;
Char_t  StFlowEvent::mPid[10]              = {'\0'};

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

Double_t StFlowEvent::PhiWeight(Int_t selN, Int_t harN,	StFlowTrack*
				pFlowTrack) const {
  // Weight for making the event plane isotropic in the lab.

  bool oddHar = (harN+1) % 2;
  StTrackTopologyMap topologyMap = pFlowTrack->TopologyMap();
  float phi = pFlowTrack->Phi();
  if (phi < 0.) phi += twopi;
  float eta = pFlowTrack->Eta();

  Double_t phiWgt;
  int n;

  if (topologyMap.numberOfHits(kTpcId) || // Tpc track, or no topologyMap
      topologyMap.data(0) == 0 && topologyMap.data(1) == 0) {
    n = (int)((phi/twopi)*Flow::nPhiBins);
    if (mOnePhiWgt) {
      phiWgt = mPhiWgt[selN][harN][n];
    } else {
      float vertexZ = mVertexPos.z();
      if (eta > 0. && vertexZ > 0.) {
	phiWgt = mPhiWgtFarWest[selN][harN][n];
      } else if (eta > 0. && vertexZ < 0.) {
	phiWgt = mPhiWgtWest[selN][harN][n];
      } else if (eta < 0. && vertexZ > 0.) {
	phiWgt = mPhiWgtEast[selN][harN][n];
      } else {
	phiWgt = mPhiWgtFarEast[selN][harN][n];
      }      
    }
  }

  else if (topologyMap.numberOfHits(kFtpcEastId)) { // Ftpc east track
    n = (int)((phi/twopi)*Flow::nPhiBinsFtpc);
    phiWgt = mPhiWgtFtpcEast[selN][harN][n];
  }
  
  else if (topologyMap.numberOfHits(kFtpcWestId)) { // Ftpc west track
    n = (int)((phi/twopi)*Flow::nPhiBinsFtpc);
    phiWgt = mPhiWgtFtpcWest[selN][harN][n];
  }

  if (mPtWgt) {
    float pt = pFlowTrack->Pt();
    phiWgt *= (pt < 2.) ? pt : 2.;  // pt weighting going constant above 2 GeV
  }

  float etaAbs = fabs(eta);
  if (mEtaWgt && oddHar && etaAbs > 1.) {
    phiWgt *= etaAbs;
  }
  
  if (oddHar && eta < 0.) phiWgt *= -1.;

  return phiWgt;
}

//-------------------------------------------------------------

UInt_t StFlowEvent::Mult(StFlowSelection* pFlowSelect) {
  // Multiplicity of tracks selected for the event plane

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

Float_t StFlowEvent::MeanPt(StFlowSelection* pFlowSelect) {
  // Mean pt of tracks selected for the event plane

  float sumPt = 0.;
  UInt_t mult = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      sumPt += pFlowTrack->Pt();
      mult++;
    }
  }
  
  return (mult) ? sumPt/(float)mult : 0.;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::Q(StFlowSelection* pFlowSelect) {
  // Event plane vector
 
  TVector2 mQ;
  Double_t mQx=0., mQy=0.;
  int    selN  = pFlowSelect->Sel();
  int    harN  = pFlowSelect->Har();
  double order = (double)(harN + 1);

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      double phiWgt = PhiWeight(selN, harN, pFlowTrack);
      float phi = pFlowTrack->Phi();
      mQx += phiWgt * cos(phi * order);
      mQy += phiWgt * sin(phi * order);
    }
  }
  mQ.Set(mQx, mQy);

  return mQ;
}

//-------------------------------------------------------------

Float_t StFlowEvent::Psi(StFlowSelection* pFlowSelect) {
  // Event plane angle

  int    harN = pFlowSelect->Har();
  float order = (float)(harN + 1);
  Float_t psi = 0.;

  TVector2 mQ = Q(pFlowSelect);

  if (mQ.Mod()) {
    psi= mQ.Phi() / order;
    if (psi < 0.) { psi += twopi / order; }
  }
  
  return psi;
}

//-----------------------------------------------------------------------

Double_t StFlowEvent::G_New(StFlowSelection* pFlowSelect, Double_t Zx, Double_t Zy) { 
  // Generating function for the new cumulant method. Eq. 3 in the Practical Guide.

  int selN     = pFlowSelect->Sel();
  int harN     = pFlowSelect->Har();
  double order = (double)(harN + 1);

  double mult = (double)Mult(pFlowSelect);
  Double_t theG = 1.;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {

      double phiWgt = PhiWeight(selN, harN, pFlowTrack);      
      float phi = pFlowTrack->Phi();
      theG *= (1. + (phiWgt/mult) * (2.* Zx * cos(phi * order) + 
				     2.* Zy * sin(phi * order) ) );            
    }
  }

  return theG;
}

//-----------------------------------------------------------------------

Double_t StFlowEvent::G_Old(StFlowSelection* pFlowSelect, Double_t Zx, Double_t Zy) { 
  // Generating function for the old cumulant method.
  // If expanded in Taylor series, one recovers G_New() in new new cumulant method.

  TVector2 normQ = NormQ(pFlowSelect);

  return exp(2*Zx*normQ.X() + 2*Zy*normQ.Y());

}

//-------------------------------------------------------------

TVector2 StFlowEvent::NormQ(StFlowSelection* pFlowSelect) { 
  // Return normalized Q = Q / sqrt(sum of weights**2)

  TVector2 mQ;
  Double_t mQx=0., mQy=0.;
  int selN     = pFlowSelect->Sel();
  int harN     = pFlowSelect->Har();
  double order = (double)(harN + 1);
  double SumOfWeightSqr = 0;


  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      
      double phiWgt = PhiWeight(selN, harN, pFlowTrack);
      SumOfWeightSqr += phiWgt*phiWgt;

      float phi = pFlowTrack->Phi();
      mQx += phiWgt * cos(phi * order);
      mQy += phiWgt * sin(phi * order);
    }
  }
  
  if (SumOfWeightSqr)
    mQ.Set(mQx/sqrt(SumOfWeightSqr), mQy/sqrt(SumOfWeightSqr));
  else mQ.Set(0.,0.);
  
  return mQ;
}

//-------------------------------------------------------------

Double_t StFlowEvent::SumWeightSquare(StFlowSelection* pFlowSelect) {
 // Return sum of weights**2

  int selN = pFlowSelect->Sel();
  int harN = pFlowSelect->Har();
  Double_t SumOfWeightSqr = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      
      double phiWgt = PhiWeight(selN, harN, pFlowTrack);
      SumOfWeightSqr += phiWgt*phiWgt;
    }
  }

  if (SumOfWeightSqr < 0.) return Mult(pFlowSelect);

  return SumOfWeightSqr;
}

//-------------------------------------------------------------

Double_t StFlowEvent::WgtMult_q4(StFlowSelection* pFlowSelect) { 
  // Used only for the old cumulant method, for getting q4 when weight is on.
  // Replace multiplicity in Eq.(74b) by this quantity when weight is on.
  // This is derived based on (A4) in the old cumulant paper.

  int selN = pFlowSelect->Sel();
  int harN = pFlowSelect->Har();
  double theMult        = 0.;
  double theMeanWj4     = 0.;
  double theMeanWj2     = 0.;
  double theSumOfWgtSqr = 0;
  double phiWgtSq;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      
      double phiWgt   = PhiWeight(selN, harN, pFlowTrack);
      phiWgtSq        = phiWgt*phiWgt;
      theSumOfWgtSqr += phiWgtSq;
      theMeanWj4     += phiWgtSq*phiWgtSq;
      theMult        += 1.;      
    }
  }
  
  if (theMult <= 0.) return theMult;

  theMeanWj4 /= theMult;
  theMeanWj2  = theSumOfWgtSqr / theMult;

  return (theSumOfWgtSqr*theSumOfWgtSqr)/(theMult*(-theMeanWj4+2*theMeanWj2*theMeanWj2));
}

//-------------------------------------------------------------

Double_t StFlowEvent::WgtMult_q6(StFlowSelection* pFlowSelect) { 
  // Used only for the old cumulant method. For getting q6 when weight is on.
  // Replace multiplicity in Eq.(74c) by this quantity when weight is on.
  // This is derived based on (A4) in the old cumulant paper.

  int selN = pFlowSelect->Sel();
  int harN = pFlowSelect->Har();
  double theMult        = 0.;
  double theMeanWj6     = 0.;
  double theMeanWj4     = 0.;
  double theMeanWj2     = 0.;
  double theSumOfWgtSqr = 0;
  double phiWgtSq;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      
      double phiWgt   = PhiWeight(selN, harN, pFlowTrack);
      phiWgtSq        = phiWgt*phiWgt;
      theSumOfWgtSqr += phiWgtSq;
      theMeanWj4     += phiWgtSq*phiWgtSq;
      theMeanWj6     += phiWgtSq*phiWgtSq*phiWgtSq;
      theMult        += 1.;
    }
  }
    
  if (theMult <= 0.) return theMult*theMult;

  theMeanWj6 /= theMult;
  theMeanWj4 /= theMult;
  theMeanWj2  = theSumOfWgtSqr / theMult;

  return 4.*(theSumOfWgtSqr*theSumOfWgtSqr*theSumOfWgtSqr)/(theMult*(theMeanWj6-9.*theMeanWj2*theMeanWj4+12.*theMeanWj2*theMeanWj2*theMeanWj2));
}

//-------------------------------------------------------------

Float_t StFlowEvent::q(StFlowSelection* pFlowSelect) {
  // Magnitude of normalized Q vector
 
  TVector2 mQ  = NormQ(pFlowSelect);
  
  return mQ.Mod();
}

//-----------------------------------------------------------------------

void StFlowEvent::SetSelections() {
  // for particles used for the event plane

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    double eta = (double)(pFlowTrack->Eta());
    float  Pt  = pFlowTrack->Pt();

    // PID
    if (mPid[0] != '\0') {
      if (strstr(mPid, "h")!=0) {
	int charge = pFlowTrack->Charge();
	if (strcmp("h+", mPid)==0 && charge != 1)  continue;
	if (strcmp("h-", mPid)==0 && charge != -1) continue;
      } else {
	Char_t pid[10];
	strcpy(pid, pFlowTrack->Pid());
	if (strstr(pid, mPid)==0) continue;
      }
    }

    // Global DCA
    float gDca = pFlowTrack->DcaGlobal();
    if (mDcaGlobalCuts[1] > mDcaGlobalCuts[0] &&
	(gDca < mDcaGlobalCuts[0] || gDca >= mDcaGlobalCuts[1])) continue;
    
    for (int selN = 0; selN < Flow::nSels; selN++) {
      for (int harN = 0; harN < Flow::nHars; harN++) {
	    
	  if (pFlowTrack->TopologyMap().numberOfHits(kTpcId) ||  
	      (pFlowTrack->TopologyMap().data(0) == 0 && 
	       pFlowTrack->TopologyMap().data(1) == 0)) {
	    // hits in Tpc or TopologyMap not available
	    
	    // Eta
	    if (mEtaTpcCuts[1][harN][selN] > mEtaTpcCuts[0][harN][selN] && 
		(fabs(eta) < mEtaTpcCuts[0][harN][selN] || 
		 fabs(eta) >= mEtaTpcCuts[1][harN][selN])) continue;
	    // 	    (eta < mEtaTpcCuts[0][harN][selN]         || both subs at +eta
	    // 	     eta >= mEtaTpcCuts[1][harN][selN])) continue;
	    
	    // Pt
	    if (mPtTpcCuts[1][harN][selN] > mPtTpcCuts[0][harN][selN] && 
		(Pt < mPtTpcCuts[0][harN][selN] ||
		 Pt >= mPtTpcCuts[1][harN][selN])) continue;
	  }
	  
	  else if (pFlowTrack->TopologyMap().numberOfHits(kFtpcEastId) || 
		   pFlowTrack->TopologyMap().numberOfHits(kFtpcWestId)) {
	    // hits in Ftpc
	    
	    // Eta
	    if (mEtaFtpcCuts[1][harN][selN] > mEtaFtpcCuts[0][harN][selN] && 
		(fabs(eta) < mEtaFtpcCuts[0][harN][selN] || 
		 fabs(eta) >= mEtaFtpcCuts[1][harN][selN])) continue;
	    // 	    (eta < mEtaTpcCuts[0][harN][selN]         || both subs at +eta
	    // 	     eta >= mEtaTpcCuts[1][harN][selN])) continue;
	    
	    // Pt
	    if (mPtFtpcCuts[1][harN][selN] > mPtFtpcCuts[0][harN][selN] && 
		(Pt < mPtFtpcCuts[0][harN][selN] ||
		 Pt >= mPtFtpcCuts[1][harN][selN])) continue;	
	  }
	  
	  pFlowTrack->SetSelect(harN, selN);
	  
      }
    }
  }
  
}

//-------------------------------------------------------------

void StFlowEvent::MakeSubEvents() {
  // Make random subevents

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

void StFlowEvent::MakeEtaSubEvents() {
  // Make subevents with positive and negative eta

  StFlowTrackIterator itr;
  int harN, selN = 0;
  
  // loop to set the SubEvent member variable
  for (selN = 0; selN < Flow::nSels; selN++) {
    for (harN = 0; harN < Flow::nHars; harN++) {
      for (itr = TrackCollection()->begin(); 
           itr != TrackCollection()->end(); itr++) {
	StFlowTrack* pFlowTrack = *itr;
	if (pFlowTrack->Select(harN, selN)) {
	  float eta = pFlowTrack->Eta();
	  if (eta > 0.) {
	    pFlowTrack->SetSubevent(harN, selN, 0);
	  } else {
	    pFlowTrack->SetSubevent(harN, selN, 1);
	  }
	}
      }
    }
  }
  
}

//-----------------------------------------------------------------------

void StFlowEvent::SetPidsDeviant() {
  // Set PID with PID Deviant method
  
  StFlowTrackIterator itr;

  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {

    StFlowTrack* pFlowTrack = *itr;
    Char_t pid[10] = "none";
    Short_t charge = pFlowTrack->Charge();

    bool bPiPlus       = kFALSE;
    bool bPiMinus      = kFALSE;
    bool bProton       = kFALSE;
    bool bAntiProton   = kFALSE;
    bool bKplus        = kFALSE;
    bool bKminus       = kFALSE;
    bool bDeuteron     = kFALSE;
    bool bAntiDeuteron = kFALSE;
    bool bElectron     = kFALSE;
    bool bPositron     = kFALSE;
    
    if (charge == 1) {
      float piPlus    = pFlowTrack->PidPiPlus();
      float proton    = pFlowTrack->PidProton();
      float kPlus     = pFlowTrack->PidKaonPlus();
      float deuteron  = pFlowTrack->PidDeuteron();
      float positron  = pFlowTrack->PidPositron();
      if (piPlus > mPiPlusCuts[0] && 
	  piPlus < mPiPlusCuts[1]) {
	bPiPlus = kTRUE;
      } 
      if ( proton > mProtonCuts[0] && 
	   proton < mProtonCuts[1]) {
	bProton = kTRUE;
      } 
      if ( kPlus > mKPlusCuts[0] && 
	   kPlus < mKPlusCuts[1]) {
	bKplus = kTRUE;
      } 
      if ( deuteron > mDeuteronCuts[0] && 
	   deuteron < mDeuteronCuts[1]) {
	bDeuteron = kTRUE;
      } 
      if ( positron > mPositronCuts[0] && 
	   positron < mPositronCuts[1]) {
	bPositron = kTRUE;
      }
    } else if (charge == -1) {
      float piMinus      = pFlowTrack->PidPiMinus();
      float antiProton   = pFlowTrack->PidAntiProton();
      float kMinus       = pFlowTrack->PidKaonMinus();
      float antiDeuteron = pFlowTrack->PidAntiDeuteron();
      float electron     = pFlowTrack->PidElectron();
      if (piMinus > mPiMinusCuts[0] && 
	  piMinus < mPiMinusCuts[1]) {
	bPiMinus = kTRUE;
      } 
      if ( antiProton > mAntiProtonCuts[0] && 
	   antiProton < mAntiProtonCuts[1]) {
	bAntiProton = kTRUE;
      } 
      if ( kMinus > mKMinusCuts[0] && 
	   kMinus < mKMinusCuts[1]) {
	bKminus = kTRUE;
      } 
      if ( antiDeuteron > mAntiDeuteronCuts[0] && 
	   antiDeuteron < mAntiDeuteronCuts[1]) {
	bAntiDeuteron = kTRUE;
      } 
      if ( electron > mElectronCuts[0] && 
	   electron < mElectronCuts[1]) {
	bElectron = kTRUE;
      }
    }

    if (bPiPlus && !bPiMinus && !bProton && !bAntiProton && 
	!bKplus && !bKminus && !bDeuteron && !bAntiDeuteron && 
	!bElectron && !bPositron) { strcpy(pid, "pi+"); } 
    if (!bPiPlus && bPiMinus && !bProton && !bAntiProton && 
	!bKplus && !bKminus && !bDeuteron && !bAntiDeuteron && 
	!bElectron && !bPositron) { strcpy(pid, "pi-"); } 
    if (!bPiPlus && !bPiMinus && bProton && !bAntiProton && 
	!bKplus && !bKminus && !bDeuteron && !bAntiDeuteron && 
	!bElectron && !bPositron) { strcpy(pid, "pr+"); } 
    if (!bPiPlus && !bPiMinus && !bProton && bAntiProton && 
	!bKplus && !bKminus && !bDeuteron && !bAntiDeuteron && 
	!bElectron && !bPositron) { strcpy(pid, "pr-"); } 
    if (!bPiPlus && !bPiMinus && !bProton && !bAntiProton && 
	bKplus && !bKminus && !bDeuteron && !bAntiDeuteron && 
	!bElectron && !bPositron) { strcpy(pid, "k+"); } 
    if (!bPiPlus && !bPiMinus && !bProton && !bAntiProton && 
	!bKplus && bKminus && !bDeuteron && !bAntiDeuteron && 
	!bElectron && !bPositron) { strcpy(pid, "k-"); } 
    if (!bPiPlus && !bPiMinus && !bProton && !bAntiProton && 
	!bKplus && !bKminus && bDeuteron && !bAntiDeuteron && 
	!bElectron && !bPositron) { strcpy(pid, "d+"); } 
    if (!bPiPlus && !bPiMinus && !bProton && !bAntiProton && 
	!bKplus && !bKminus && !bDeuteron && bAntiDeuteron && 
	!bElectron && !bPositron) { strcpy(pid, "d-"); } 
    if (!bPiPlus && !bPiMinus && !bProton && !bAntiProton && 
	!bKplus && !bKminus && !bDeuteron && !bAntiDeuteron && 
	bElectron && !bPositron) { strcpy(pid, "e-"); } 
    if (!bPiPlus && !bPiMinus && !bProton && !bAntiProton && 
	!bKplus && !bKminus && !bDeuteron && !bAntiDeuteron && 
	!bElectron && bPositron) { strcpy(pid, "e+"); } 

    pFlowTrack->SetPid(pid);

  }
}

//-----------------------------------------------------------------------

void StFlowEvent::SetPidsProb() {
  // Set PID with PID Probability method
  
  StFlowTrackIterator itr;

  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {

    StFlowTrack* pFlowTrack = *itr;
    Char_t pid[10] = "none";

    if (pFlowTrack->MostLikelihoodPID() == 8 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "pi+"); } 
    if (pFlowTrack->MostLikelihoodPID() == 9 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "pi-"); } 
    if (pFlowTrack->MostLikelihoodPID() == 14 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "pr+"); } 
    if (pFlowTrack->MostLikelihoodPID() == 15 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "pr-"); } 
    if (pFlowTrack->MostLikelihoodPID() == 11 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "k+"); } 
    if (pFlowTrack->MostLikelihoodPID() == 12 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "k-"); } 
    if (pFlowTrack->MostLikelihoodPID() == 45 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "d+"); } 
//      if (pFlowTrack->MostLikelihoodPID() == &&  
//  	pFlowTrack->MostLikelihoodProb() > 0.9)
//      { strcpy(pid, "d-"); } 
    if (pFlowTrack->MostLikelihoodPID() == 3 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "e-"); } 
    if (pFlowTrack->MostLikelihoodPID() == 2 &&  
	pFlowTrack->MostLikelihoodProb() > 0.9)
      { strcpy(pid, "e+"); } 

    pFlowTrack->SetPid(pid);

  }
}

//-----------------------------------------------------------------------

void StFlowEvent::SetCentrality() {

  Int_t* cent;
  Int_t  tracks = mMultEta; // converts UInt_t to Int_t

  if (mCenterOfMassEnergy == 0.) { // year=1
    cent = Flow::cent130;
  } else if (mCenterOfMassEnergy >= 199.) {
    if (fabs(mMagneticField) >= 4.) { // year=2, Au+Au, Full Field
      cent = Flow::cent200Full;
    } else { // year=2, Au+Au, Half Field
      cent = Flow::cent200Half;
    }
  } else if (mCenterOfMassEnergy <= 25.){ // year=2, 22 GeV
    cent = Flow::cent22;
  }

  if      (tracks < cent[0])  { mCentrality = 0; }
  else if (tracks < cent[1])  { mCentrality = 1; }
  else if (tracks < cent[2])  { mCentrality = 2; }
  else if (tracks < cent[3])  { mCentrality = 3; }
  else if (tracks < cent[4])  { mCentrality = 4; }
  else if (tracks < cent[5])  { mCentrality = 5; }
  else if (tracks < cent[6])  { mCentrality = 6; }
  else if (tracks < cent[7])  { mCentrality = 7; }
  else if (tracks < cent[8])  { mCentrality = 8; }
  else                        { mCentrality = 9; }
}

//-----------------------------------------------------------------------

void StFlowEvent::PrintSelectionList() {
  // Prints the list of selection cuts
  // Call in Finish

  cout << "#######################################################" << endl;
  cout << "# Weighting and Striping:" << endl;
  if (mPtWgt) {
    cout << "#    PtWgt= TRUE, also for output of PhiWgt file" << endl;
  } else {
    cout << "#    PtWgt= FALSE" << endl;
  }
  if (mEtaWgt) {
    cout << "#    EtaWgt= TRUE, also for output of PhiWgt file for odd harmonics" << endl;
  } else {
    cout << "#    EtaWgt= FALSE" << endl;
  }
  if (mEtaSubs) {
    cout << "#    EtaSubs= TRUE" << endl;
  } else {
    cout << "#    EtaSubs= FALSE" << endl;
  }
  cout << "#######################################################" << endl;
  cout << "# Pid Deviant Cuts:" << endl; 
  cout << "#    PiPlus cuts=  " << mPiPlusCuts[0] << ", " 
       << mPiPlusCuts[1] << endl;
  cout << "#    PiMinus cuts= " << mPiMinusCuts[0] << ", " 
       << mPiMinusCuts[1] << endl;
  cout << "#    Proton cuts=  " << mProtonCuts[0] << ", " 
       << mProtonCuts[1] << endl;
  cout << "#    Anti Proton cuts=  " << mAntiProtonCuts[0] << ", " 
       << mAntiProtonCuts[1] << endl;
  cout << "#    Deuteron cuts=  " << mDeuteronCuts[0] << ", " 
       << mDeuteronCuts[1] << endl;
  cout << "#    Anti Deuteron cuts=  " << mAntiDeuteronCuts[0] << ", " 
       << mAntiDeuteronCuts[1] << endl;
  cout << "#    K- cuts=  " << mKMinusCuts[0] << ", " 
       << mKMinusCuts[1] << endl;
  cout << "#    K+ cuts=  " << mKPlusCuts[0] << ", " 
       << mKPlusCuts[1] << endl;
  cout << "#    Electron cuts=  " << mElectronCuts[0] << ", " 
       << mElectronCuts[1] << endl;
  cout << "#    Positron cuts=  " << mPositronCuts[0] << ", " 
       << mPositronCuts[1] << endl;
  cout << "#######################################################" << endl;
  cout << "# Tracks used for the event plane:" << endl; 
  cout << "# Particle ID= " << mPid << endl; 
  cout << "# Global Dca cuts= " << mDcaGlobalCuts[0] << ", " 
       << mDcaGlobalCuts[1] << endl;
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      cout << "#  selection= " << k+1 << " harmonic= " 
	   << j+1 << endl;
      cout << "#    abs(Eta) Tpc cuts= " << mEtaTpcCuts[0][j][k] << ", " 
	   << mEtaTpcCuts[1][j][k] << endl;
      cout << "#    abs(Eta) Ftpc cuts= " << mEtaFtpcCuts[0][j][k] << ", " 
	   << mEtaFtpcCuts[1][j][k] << endl;
      cout << "#    Pt Tpc cuts= " << mPtTpcCuts[0][j][k] << ", "
	   << mPtTpcCuts[1][j][k] << endl;
      cout << "#    Pt Ftpc cuts= " << mPtFtpcCuts[0][j][k] << ", "
	   << mPtTpcCuts[1][j][k] << endl;
    }
  }
  cout << "#######################################################" << endl;
  
}

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.cxx,v $
// Revision 1.34  2002/06/10 22:50:59  posk
// pt and eta weighting now default.
// DcaGlobalPart default now 0 to 1 cm.
// Event cut order changed.
//
// Revision 1.33  2002/05/23 18:54:10  posk
// Moved centrality cuts into StFlowConstants
//
// Revision 1.32  2002/03/15 16:43:21  snelling
// Added a method to recalculate the centrality in StFlowPicoEvent
//
// Revision 1.31  2002/03/14 18:51:49  snelling
// Added new centralities
//
// Revision 1.30  2002/02/13 22:29:21  posk
// Pt Weight now also weights Phi Weights. Added Eta Weight, default=FALSE.
//
// Revision 1.29  2002/01/31 01:04:43  posk
// *** empty log message ***
//
// Revision 1.28  2001/12/18 19:22:02  posk
// "proton" and "antiproton" changed to "pr+" and "pr-".
// Compiles on Solaris.
//
// Revision 1.27  2001/12/11 21:33:43  posk
// Went from one to four sets of histograms for making the event plane isotropic.
// StFlowEvent::PhiWeight() has changed arguments and return value.
// The ptWgt saturates above 2 GeV/c.
//
// Revision 1.26  2001/11/09 21:10:37  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.25  2001/11/02 04:49:52  aihong
// add func. for cumulant maker
//
// Revision 1.24  2001/08/08 10:35:07  oldi
// Typo in output statement of cut lists fixed (mEtaTpcCuts[1][j][k] -> mEtaFtpcCuts[1][j][k]).
//
// Revision 1.23  2001/06/07 20:06:16  posk
// Global Dca cut for event plane particles.
// Removed SetPtWgt().
//
// Revision 1.22  2001/05/22 20:17:26  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.21  2001/04/03 17:47:17  oldi
// Bug fix that excluded FTPC tracks from the determination of the reaction plane.
//
// Revision 1.20  2000/12/12 20:22:05  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.19  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.18  2000/12/08 17:03:38  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.17  2000/10/12 22:46:35  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.16  2000/09/26 20:51:37  posk
// Updated documentation.
//
// Revision 1.14  2000/09/15 22:51:28  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.13  2000/09/12 01:30:23  snelling
// Changed PID selection
//
// Revision 1.12  2000/09/05 16:11:31  snelling
// Added global DCA, electron and positron
//
// Revision 1.11  2000/08/31 18:58:21  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.10  2000/08/12 20:22:19  posk
// Recalculate centrality in read from pico.
//
// Revision 1.9  2000/08/10 23:00:21  posk
// New centralities. pt and eta cuts.
//
// Revision 1.8  2000/08/09 21:38:23  snelling
// PID added
//
// Revision 1.7  2000/06/01 18:26:35  posk
// Increased precision of Track integer data members.
//
// Revision 1.5  2000/05/16 20:59:29  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.4  2000/05/12 22:42:04  snelling
// Additions for persistency and minor fix
//
// Revision 1.2  2000/03/15 23:28:50  posk
// Added StFlowSelection.
//
// Revision 1.1  2000/03/02 23:02:48  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.16  2000/02/29 22:00:53  posk
// Made SetPhiWeight inline, changed ImpactPar to Dca, etc.
//
// Revision 1.15  2000/02/29 01:26:11  snelling
// removed static const int& nxxx = Flow::nxxx;
//
// Revision 1.14  2000/02/18 22:49:54  posk
// Added PID and centrality.
//
// Revision 1.9  1999/12/21 17:31:50  posk
// Fixed random_shuffle in making the sub events.
//
// Revision 1.6  1999/12/15 22:01:25  posk
// Added StFlowConstants.hh
//
// Revision 1.4  1999/12/04 00:10:32  posk
// Works with the new StEvent
//
// Revision 1.3  1999/11/30 18:52:51  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:13  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/04 19:02:04  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////
