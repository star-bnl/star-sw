//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.cxx,v 1.63 2010/09/30 19:30:26 posk Exp $
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

#include <Stiostream.h>
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
#include "TComplex.h"
#include "TH1.h"
#define PR(x) cout << "##### FlowEvent: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowEvent)
Double_t  StFlowEvent::mZDCSMDCenterex = Flow::zdcsmd_ex0;
Double_t  StFlowEvent::mZDCSMDCenterey = Flow::zdcsmd_ey0;
Double_t  StFlowEvent::mZDCSMDCenterwx = Flow::zdcsmd_wx0;
Double_t  StFlowEvent::mZDCSMDCenterwy = Flow::zdcsmd_wy0;

// CumalentMaker:
// Main TPC particles do not participate in G Mix  calc. for the 1st Har. sel2
// for sel 1, C{3} is ( har1(all) + har1(all) + har2(all) )
//           differential v1{3} is ( har1(all) + har1(all) + har2(all) ) 
// 
// for sel 2, C{3} is ( har1(FTPC)+ har1(FTPC)+ har2(TPC) )
//           differential v1{3} is ( har1(FTPC)+ har1(FTPC)+ har2(TPC) )
//
// Please note that for the mixed har analysis, only the 1st har. make sense anyway.

Float_t  StFlowEvent::mV1TPCDetctWgtG_Mix[Flow::nSels]      ={1., 0.};
Float_t  StFlowEvent::mV1FtpcWestDetctWgtG_Mix[Flow::nSels] ={1., 1.};
Float_t  StFlowEvent::mV1FtpcEastDetctWgtG_Mix[Flow::nSels] ={1., 1.};

Float_t  StFlowEvent::mV2TPCDetctWgtG_Mix[Flow::nSels]      ={1., 1.};
Float_t  StFlowEvent::mV2FtpcWestDetctWgtG_Mix[Flow::nSels] ={1., 0.};
Float_t  StFlowEvent::mV2FtpcEastDetctWgtG_Mix[Flow::nSels] ={1., 0.};

Float_t  StFlowEvent::mEtaTpcCuts[2][2][Flow::nSels] = {{{0.,0.5},
                                                         {0.,0.} },
							{{1.0,2.},
							 {1.0,1.} }};
// Float_t  StFlowEvent::mEtaTpcCuts[2][2][Flow::nSels] = {{{0.,0.},
//                                                          {0.,0.} },
// 							{{1.0,1.},
// 							 {1.0,1.} }};
Float_t  StFlowEvent::mEtaFtpcCuts[4][2][Flow::nSels] = {{{-4.0,-4.0},
							  {-4.0,-4.0}},
							 {{-2.7,-2.7},
							  {-2.7,-2.7} },
							 {{2.7,2.7},
							  {2.7,2.7} },
							 {{4.0,4.0},
							  {4.0,4.0} }};

Float_t  StFlowEvent::mPtTpcCuts[2][2][Flow::nSels] =  {{{0.15,0.15},
							 {0.15,0.15} },
							{{2.,2.},
							 {2.,2.} }};
Float_t  StFlowEvent::mPtFtpcCuts[2][2][Flow::nSels] = {{{0.15,0.15},
						         {0.15,0.15} },
						        {{2.,2.},
						         {2.,2.} }};

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
Float_t StFlowEvent::mDcaGlobalTpcCuts[2]  = { 0., 0.};
Float_t StFlowEvent::mDcaGlobalFtpcCuts[2] = { 0., 0.};
Float_t StFlowEvent::mPtWgtSaturation      = 2.;
Bool_t  StFlowEvent::mPtWgt                = kTRUE;
Bool_t  StFlowEvent::mEtaWgt               = kTRUE;
Bool_t  StFlowEvent::mProbPid              = kFALSE;
Bool_t  StFlowEvent::mEtaSubs              = kFALSE;
Bool_t  StFlowEvent::mRanSubs              = kFALSE;
Bool_t  StFlowEvent::mFirstLastPhiWgt      = kFALSE;
Bool_t  StFlowEvent::mFirstLastPoints      = kFALSE;
Bool_t  StFlowEvent::mUseZDCSMD		   = kFALSE;
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

Double_t StFlowEvent::PhiWeightRaw(Int_t selN, Int_t harN, StFlowTrack*
				pFlowTrack) const {
  // Weight for making the event plane isotropic in the lab.

  bool oddHar = (harN+1) % 2; // Choose odd or even
  harN = oddHar ? 0 : 1;

  StTrackTopologyMap map = pFlowTrack->TopologyMap();
  float phi = pFlowTrack->Phi();
  if (phi < 0.) phi += twopi;
  float eta = pFlowTrack->Eta();

  Double_t phiWgt = 0.;
  int n = 0;

  float vertexZ = mVertexPos.z();
  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) { 
    // Tpc track, or no topologyMap
    n = (int)((phi/twopi)*Flow::nPhiBins);
    if (mFirstLastPhiWgt) {
      float zFirstPoint = pFlowTrack->ZFirstPoint();
      float zLastPoint  = pFlowTrack->ZLastPoint();
      if (zFirstPoint > 0. && zLastPoint > 0.) {
	phiWgt = mPhiWgtFarWest[selN][harN][n];
      } else if (zFirstPoint > 0. && zLastPoint < 0.) {
	phiWgt = mPhiWgtWest[selN][harN][n];
      } else if (zFirstPoint < 0. && zLastPoint > 0.) {
	phiWgt = mPhiWgtEast[selN][harN][n];
      } else {
	phiWgt = mPhiWgtFarEast[selN][harN][n];
      }
    } else {      
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

  else if (map.trackFtpcEast()) { // Ftpc east track == eta < 0.
    n = (int)((phi/twopi)*Flow::nPhiBinsFtpc);
    if (vertexZ < 0.) {
      phiWgt = mPhiWgtFtpcFarEast[selN][harN][n];
    }
    else { // vertexZ > 0.
      phiWgt = mPhiWgtFtpcEast[selN][harN][n];
    }
  }
  
  else if (map.trackFtpcWest()) { // Ftpc west track == eta > 0.
    n = (int)((phi/twopi)*Flow::nPhiBinsFtpc);
    if (vertexZ > 0.) {
      phiWgt = mPhiWgtFtpcFarWest[selN][harN][n];
    }
    else { // vertexZ < 0.
      phiWgt = mPhiWgtFtpcWest[selN][harN][n];
    }
  }

  return phiWgt;
}

//-------------------------------------------------------------

Double_t StFlowEvent::Weight(Int_t selN, Int_t harN, StFlowTrack*
			     pFlowTrack) const {
  // Weight for enhancing the resolution.

  //bool oddHar = (harN+1) % 2;
  Double_t wgt = 1.;
  wgt *= PtAbsWgtValue(pFlowTrack->Pt());
  float eta = pFlowTrack->Eta();
  wgt *= EtaAbsWgtValue(eta);
  //if (oddHar && eta < 0.) { wgt *= -1. ; }
  if (harN == 0 && eta < 0.) { wgt *= -1. ; } // reverse only for 1st har

  return wgt;
}

//-------------------------------------------------------------

Double_t StFlowEvent::PtAbsWgtValue(Double_t pt) const {

  return  ((mPtWgt) ? ((pt < mPtWgtSaturation) ? pt : mPtWgtSaturation) : 1.);
}

//-------------------------------------------------------------

Double_t StFlowEvent::EtaAbsWgtValue(Double_t eta) const {

 return  ((mEtaWgt) ? ((fabs(eta)<0.005) ? 0.005 : fabs(eta)) : 1.); 
}

//-----------------------------------------------------------------------

Double_t StFlowEvent::PhiWeight(Int_t selN, Int_t harN, StFlowTrack*
				pFlowTrack) const {
  // Weight for making the event plane isotropic in the lab and
  // enhancing the resolution.
  
  Double_t phiWgtRaw = PhiWeightRaw(selN, harN, pFlowTrack);
  Double_t weight = Weight(selN, harN, pFlowTrack);
  
  return phiWgtRaw * weight;
}

//-------------------------------------------------------------

Double_t StFlowEvent::ZDCSMD_PsiWgtEast() {
  // Get psi weight for east ZDCSMD

  TH1F *mZDCSMD_PsiWgt = new TH1F("ZDCSMD_PsiWgt","ZDCSMD_PsiWgt",
				  Flow::zdcsmd_nPsiBins,-twopi/2.,twopi/2.);
  Int_t n = mZDCSMD_PsiWgt->FindBin(ZDCSMD_PsiEst());
  mZDCSMD_PsiWgt->Delete();

  return mZDCSMD_PsiWgtEast[n-1];
}

//-------------------------------------------------------------

Double_t StFlowEvent::ZDCSMD_PsiWgtWest() {
  // Get psi weight for west ZDCSMD

  TH1F *mZDCSMD_PsiWgt = new TH1F("ZDCSMD_PsiWgt","ZDCSMD_PsiWgt",
				  Flow::zdcsmd_nPsiBins,-twopi/2.,twopi/2.);
  Int_t n = mZDCSMD_PsiWgt->FindBin(ZDCSMD_PsiWst());
  mZDCSMD_PsiWgt->Delete();

  return mZDCSMD_PsiWgtWest[n-1];
}

//-------------------------------------------------------------
Double_t StFlowEvent::ZDCSMD_PsiWgtFull() {

  TH1F *mZDCSMD_PsiWgt=new TH1F("ZDCSMD_PsiWgt","ZDCSMD_PsiWgt",Flow::zdcsmd_nPsiBins,0.,twopi);
  StFlowSelection* mFlowSelect;
  Int_t n =mZDCSMD_PsiWgt->FindBin(Q(mFlowSelect).Phi());
  mZDCSMD_PsiWgt->Delete();

  return mZDCSMD_PsiWgtFull[n-1];
}

//-------------------------------------------------------------

UInt_t StFlowEvent::Mult(StFlowSelection* pFlowSelect) {
  // Multiplicity of tracks selected for the event plane

  UInt_t mult = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) mult++;
  }

  return mult;
}

//-------------------------------------------------------------

UInt_t StFlowEvent::MultPart(StFlowSelection* pFlowSelect) {
  // Multiplicity of tracks selected for correlation with the event plane

  UInt_t mult = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->SelectPart(pFlowTrack)) mult++;
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
 
  TVector2 mQ, reCent;
  Double_t mQx=0., mQy=0.;

  if (mUseZDCSMD) { // pFlowSelect is disabled; only 1st order Q generated
    Float_t eXsum=0., eYsum=0., wXsum=0., wYsum=0.;
    Float_t eXWgt=0., eYWgt=0., wXWgt=0., wYWgt=0.;
    for (int v=1; v<8; v++) {
      eXsum += ZDCSMD_GetPosition(0,0,v)*ZDCSMD(0,0,v);
      wXsum += ZDCSMD_GetPosition(1,0,v)*ZDCSMD(1,0,v);
      eXWgt += ZDCSMD(0,0,v);
      wXWgt += ZDCSMD(1,0,v);
    } //v 
    for (int h=1;h<9;h++) {
      eYsum += ZDCSMD_GetPosition(0,1,h)*ZDCSMD(0,1,h);
      wYsum += ZDCSMD_GetPosition(1,1,h)*ZDCSMD(1,1,h);
      eYWgt += ZDCSMD(0,1,h);
      wYWgt += ZDCSMD(1,1,h);
    }
    mQx= (eXWgt>0. && wXWgt>0. && eYWgt>0. && wYWgt>0.) ? 
      eXsum/eXWgt - wXsum/wXWgt:0.;
    mQy= (eXWgt>0. && wXWgt>0. && eYWgt>0. && wYWgt>0.) ? 
      eYsum/eYWgt - wYsum/wYWgt:0.;
  }//ZDCSMD
  else { // ana
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
	reCent = ReCentEP(selN, harN, pFlowTrack);
	mQx += (phiWgt * cos(order * phi) - reCent.X());
	mQy += (phiWgt * sin(order * phi) - reCent.Y());
      }
    }
  }//ana
  
  mQ.Set(mQx, mQy);

  return mQ;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::ReCentPar(StFlowSelection* pFlowSelect, char* TPC) {
  // For LYZ
  // Calculate weighted recentering vector per particle for each TPC
  // TPC can be "TPC", "TPCE", or "TPCW"
  // for all particles that could be correlated with the event plane
 
  TVector2 mQ;
  Double_t mQx=0., mQy=0., SumOfWeight=0.;

  int    selN  = pFlowSelect->Sel();
  int    harN  = pFlowSelect->Har();
  double order = (double)(harN + 1);
  StTrackTopologyMap map;
  
  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      map = pFlowTrack->TopologyMap();
      if ((!strcmp(TPC,"TPCE") && map.trackFtpcEast()) ||
	  (!strcmp(TPC,"TPCW") && map.trackFtpcWest()) ||
	  (!strcmp(TPC,"TPC") && map.hasHitInDetector(kTpcId))) {
	float phi = pFlowTrack->Phi();
	double wgt = fabs(Weight(selN, harN, pFlowTrack));
	mQx += wgt * cos(order * phi);
	mQy += wgt * sin(order * phi);
	SumOfWeight += wgt;
      }
    }
  }

  if (SumOfWeight)
    mQ.Set(mQx/SumOfWeight, mQy/SumOfWeight);
  else mQ.Set(0.,0.);

  return mQ;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::ReCentEPPar(StFlowSelection* pFlowSelect, char* TPC) {
  // For ana
  // Calculate the recentering vector per particle for each TPC
  // TPC can be "TPCE", "TPCW", "FTPCE", or "FTPCW"
  // for particles that are used for the event plane

  TVector2 mQ;
  Double_t mult=0., mQx=0., mQy=0.;

  int    selN  = pFlowSelect->Sel();
  int    harN  = pFlowSelect->Har();
  double order = (double)(harN + 1);
  StTrackTopologyMap map;
  
  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      map = pFlowTrack->TopologyMap();
      float eta = pFlowTrack->Eta();
      if ((!strcmp(TPC,"FTPCE") && map.trackFtpcEast()) ||
	  (!strcmp(TPC,"FTPCW") && map.trackFtpcWest()) ||
	  (!strcmp(TPC,"TPCE") && eta < 0. && map.hasHitInDetector(kTpcId)) ||
	  (!strcmp(TPC,"TPCW") && eta > 0. && map.hasHitInDetector(kTpcId)) ) {
	float phi = pFlowTrack->Phi();
	double phiWgt = PhiWeight(selN, harN, pFlowTrack);
	mQx += phiWgt * cos(order * phi);
	mQy += phiWgt * sin(order * phi);
	mult++;
      }
    }
  }

  if (mult) { mQ.Set(mQx/mult, mQy/mult); }
  else { mQ.Set(0.,0.); }

  return mQ;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::ReCent(Int_t selN, Int_t harN, StFlowTrack* pFlowTrack) const {
  // Get TVector2 for recentering in LYZ makers.

  TVector2 reCent;
  Double_t reCentX, reCentY;
  StTrackTopologyMap map = pFlowTrack->TopologyMap();
  
  if (map.hasHitInDetector(kTpcId)) {
    reCentX = mReCentX[selN][harN][2];
    reCentY = mReCentY[selN][harN][2];
  } else if (map.trackFtpcEast()) {
    reCentX = mReCentX[selN][harN][0];
    reCentY = mReCentY[selN][harN][0];
  } else if (map.trackFtpcWest()) {
    reCentX = mReCentX[selN][harN][1];
    reCentY = mReCentY[selN][harN][1];
  } else {
    reCentX = 0.;
    reCentY = 0.;
  }

  reCent.Set(reCentX, reCentY);
  return reCent;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::ReCentEP(Int_t selN, Int_t harN, StFlowTrack* pFlowTrack) const {
  // Get TVector2 for recentering in ana makers.

  TVector2 reCent;
  Double_t reCentX, reCentY;
  StTrackTopologyMap map = pFlowTrack->TopologyMap();
  
  if (map.hasHitInDetector(kTpcId)) {
    float eta = pFlowTrack->Eta();
    if (eta > 0.) { // TPCW
      reCentX = mReCentX[selN][harN][3];
      reCentY = mReCentY[selN][harN][3];
    } else { // TPCE
      reCentX = mReCentX[selN][harN][2];
      reCentY = mReCentY[selN][harN][2];
    }
  } else if (map.trackFtpcEast()) { // FTPCE
    reCentX = mReCentX[selN][harN][0];
    reCentY = mReCentY[selN][harN][0];
  } else if (map.trackFtpcWest()) { // FTPCW
    reCentX = mReCentX[selN][harN][1];
    reCentY = mReCentY[selN][harN][1];
  } else {
    reCentX = 0.;
    reCentY = 0.;
  }

  reCent.Set(reCentX, reCentY);
  return reCent;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::QPart(StFlowSelection* pFlowSelect) {
  // Event plane vector for LYZ method for all particles that could be correlated with the event plane
 
  TVector2 reCent, mQ;
  Double_t mQx=0., mQy=0.;

  int    selN  = pFlowSelect->Sel();
  int    harN  = pFlowSelect->Har();
  double order = (double)(harN + 1);
  
  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      double phiWgt = PhiWeight(selN, harN, pFlowTrack);
      float phi = pFlowTrack->Phi();
      reCent = ReCent(selN, harN, pFlowTrack);
      mQx += phiWgt * (cos(order * phi) - reCent.X());
      mQy += phiWgt * (sin(order * phi) - reCent.Y());
    }
  }
  
  mQ.Set(mQx, mQy);
  return mQ;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::NormQ(StFlowSelection* pFlowSelect) { 
  // Return normalized Q = Q / ::sqrt(sum of weights**2)
  // Where is this used?

  TVector2 mQ, reCent;
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
      reCent = ReCent(selN, harN, pFlowTrack);
      mQx += phiWgt * (cos(order * phi) - reCent.X());
      mQy += phiWgt * (sin(order * phi) - reCent.Y());
    }
  }
  
  if (SumOfWeightSqr)
    mQ.Set(mQx/::sqrt(SumOfWeightSqr), mQy/::sqrt(SumOfWeightSqr));
  else mQ.Set(0.,0.);
  
  return mQ;
}

//-------------------------------------------------------------

Float_t StFlowEvent::q(StFlowSelection* pFlowSelect) { 
  // Magnitude of normalized Q vector without pt or eta weighting

  TVector2 mQ, reCent;
  Double_t mQx=0., mQy=0.;
  int selN     = pFlowSelect->Sel();
  int harN     = pFlowSelect->Har();
  double order = (double)(harN + 1);
  double SumOfWeightSqr = 0;


  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) { // use event plane selections
      
      double phiWgt = PhiWeightRaw(selN, harN, pFlowTrack); // Raw
      SumOfWeightSqr += phiWgt*phiWgt;

      float phi = pFlowTrack->Phi();
      reCent = ReCent(selN, harN, pFlowTrack);
      mQx += phiWgt * (cos(order * phi) - reCent.X());
      mQy += phiWgt * (sin(order * phi) - reCent.Y());
    }
  }
  
  if (SumOfWeightSqr)
    mQ.Set(mQx/::sqrt(SumOfWeightSqr), mQy/::sqrt(SumOfWeightSqr));
  else mQ.Set(0.,0.);
  
  return mQ.Mod();
}

//-------------------------------------------------------------

Double_t StFlowEvent::SumWeightSquare(StFlowSelection* pFlowSelect) {

 // Return sum of weights**2

 // This method is called only by cumulant method. It won't return the true 
 // SumWeightSquare if called in standard method, in which the raw phi weight 
 // is applied. -TAH

  int selN = pFlowSelect->Sel();
  int harN = pFlowSelect->Har();
  Double_t SumOfWeightSqr = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      
      double phiWgt = Weight(selN, harN, pFlowTrack);
      SumOfWeightSqr += phiWgt*phiWgt;
    }
  }

  if (SumOfWeightSqr < 0.) return Mult(pFlowSelect);

  return SumOfWeightSqr;
}

//-------------------------------------------------------------

Float_t StFlowEvent::Qtheta(StFlowSelection* pFlowSelect, Float_t theta) {
  // Q^{\theta} for LeeYangZeros method for all particles that could be correlated with the event plane
  // BP Eq. 3 (Nucl. Phys. A 727, 373 (2003))

  Float_t Qtheta = 0.;

  int    selN  = pFlowSelect->Sel();
  int    harN  = pFlowSelect->Har();
  double order = (double)(harN + 1);
  double reCentTheta;
  TVector2 reCent;  

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      double wgt = Weight(selN, harN, pFlowTrack);
      float phi = pFlowTrack->Phi();
      reCent = ReCent(selN, harN, pFlowTrack);
      reCentTheta = reCent.X() * cos(order*theta) + reCent.Y() * sin(order*theta);
      Qtheta += wgt * (cos(order * (phi - theta)) - reCentTheta);
    }
  }

  return Qtheta;
}

//-------------------------------------------------------------

TComplex StFlowEvent::Grtheta(StFlowSelection* pFlowSelect, Float_t r, Float_t theta) {
  // Product Generating Function for LeeYangZeros method
  // PG Eq. 3 (J. Phys. G Nucl. Part. Phys 30 S1213 (2004))
 
  TComplex G   = TComplex::One();
  int    selN  = pFlowSelect->Sel();
  int    harN  = pFlowSelect->Har();
  double order = (double)(harN + 1);  
  double reCentTheta;
  TVector2 reCent;  

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      double wgt = Weight(selN, harN, pFlowTrack);
      float phi  = pFlowTrack->Phi();
      reCent = ReCent(selN, harN, pFlowTrack);
      reCentTheta = reCent.X() * cos(order*theta) + reCent.Y() * sin(order*theta);
      double Gim = r * wgt * (cos(order * (phi - theta)) - reCentTheta);
      TComplex G_i(1., Gim);
      G *= G_i;
    }
  }

  return G;
}

//-------------------------------------------------------------


TComplex StFlowEvent::GV1r0theta(StFlowSelection* pFlowSelect, Float_t r0, Float_t theta1, Float_t theta) {
  // Product Generating Function for LeeYangZeros method for v1 mixed harmonics
  // DF Eq. 1
 
  TComplex G = TComplex::One();
  double reCentTheta;
  TVector2 reCent;  

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      double wgt1 = Weight(1, 0, pFlowTrack); // selection 2, harmonic 1
      double wgt2 = Weight(1, 1, pFlowTrack); // selection 2, harmonic 2
      float  phi  = pFlowTrack->Phi();
      //reCent = ReCent(1, 0, pFlowTrack); // selection 2, harmonic 1
      reCent.Set(0.,0.);
      reCentTheta = reCent.X() * cos(1.*theta1) + reCent.Y() * sin(1.*theta1);
      double Gim1 = r0 * Flow::epsV1 * wgt1 * (cos(phi - theta1) - reCentTheta);
      //reCent = ReCent(1, 1, pFlowTrack); // selection 2, harmonic 2
      reCent.Set(0.,0.); // NOT recentered
      reCentTheta = reCent.X() * cos(2.*theta) + reCent.Y() * sin(2.*theta);
      double Gim2 = r0 * wgt2 * (cos(2*(phi - theta)) - reCentTheta);
      TComplex G_i(1., Gim1+Gim2);
      G *= G_i; 
    }
  }

  return G;
}

//-------------------------------------------------------------
TComplex StFlowEvent::Gder_r0theta(StFlowSelection* pFlowSelect, Float_t r0, Float_t theta) {
  // Sum for the denominator for diff. flow for the Product Generating Function for LeeYangZeros method
  // PG Eq. 9 (J. Phys. G Nucl. Part. Phys 30 S1213 (2004))
  // Also for v1 mixed harmonics: DF Eq. 5
  // It is the deriverative of Grtheta at r0 divided by Grtheta at r0
 
  TComplex Gder(0.,0.);
  int    selN  = pFlowSelect->Sel();
  int    harN  = pFlowSelect->Har();
  double order = (double)(harN + 1);  
  double reCentTheta;
  TVector2 reCent;  

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      double wgt = Weight(selN, harN, pFlowTrack);
      float phi  = pFlowTrack->Phi();
      reCent = ReCent(selN, harN, pFlowTrack);
      reCentTheta = reCent.X() * cos(order*theta) + reCent.Y() * sin(order*theta);
      double cosTerm = wgt * (cos(order * (phi - theta)) - reCentTheta);
      TComplex denom(1., r0*cosTerm);
      Gder += (cosTerm / denom);
    }
  }

  return Gder;
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

//-------------------------------------------------------------

Float_t StFlowEvent::ZDCSMD_PsiCorr() {
  //difference between psi's from east and west ZDCSMD

  Float_t psi_e = ZDCSMD_PsiEst();
  Float_t psi_w = ZDCSMD_PsiWst();
  Float_t psi_w_shift = psi_w + twopi/2.;

  if(psi_w_shift > twopi/2.) psi_w_shift -= twopi;
  if(psi_w_shift < -twopi/2.) psi_w_shift += twopi;

  Float_t psi_delta = psi_e - psi_w_shift;

  if(psi_delta > twopi/2.) psi_delta -= twopi;
  if(psi_delta < -twopi/2.) psi_delta += twopi;

  return psi_delta;
}

//----------------------------------------------------------------------

Float_t StFlowEvent::ZDCSMD_PsiEst() {
  //psi angle from east ZDCSMD

  Float_t eXsum=0., eYsum=0., eXWgt=0., eYWgt=0.;

  for(int v=1; v<8; v++) {
    eXsum += ZDCSMD_GetPosition(0,0,v)*ZDCSMD(0,0,v);
    eXWgt += ZDCSMD(0,0,v);
  }//v
  for(int h=1; h<9; h++) {
    eYsum += ZDCSMD_GetPosition(0,1,h)*ZDCSMD(0,1,h);
    eYWgt += ZDCSMD(0,1,h);
  }//h

  Float_t psi_e = atan2((eYWgt>0.) ? eYsum/eYWgt:0.,(eXWgt>0.) ? eXsum/eXWgt:0.);

  return psi_e;
}

//----------------------------------------------------------------------

Float_t StFlowEvent::ZDCSMD_PsiWst() {
  //psi angle from west ZDCSMD

  Float_t wXsum=0.,wYsum=0.,wXWgt=0.,wYWgt=0.;

  for(int v=1;v<8;v++) {
    wXsum += ZDCSMD_GetPosition(1,0,v)*ZDCSMD(1,0,v);
    wXWgt += ZDCSMD(1,0,v);
  }//v
  for(int h=1;h<9;h++) {
    wYsum += ZDCSMD_GetPosition(1,1,h)*ZDCSMD(1,1,h);
    wYWgt += ZDCSMD(1,1,h);
  }//h

  Float_t psi_w = atan2((wYWgt>0.) ? wYsum/wYWgt:0.,(wXWgt>0.) ? wXsum/wXWgt:0.);

  return psi_w;
}

//-----------------------------------------------------------------------

Float_t StFlowEvent::ZDCSMD_GetPosition(int eastwest,int verthori,int strip) {
  // Get position of each slat;strip starts from 1

  Float_t zdcsmd_x[7] = {0.5,2,3.5,5,6.5,8,9.5};
  Float_t zdcsmd_y[8] = {1.25,3.25,5.25,7.25,9.25,11.25,13.25,15.25};

  if(eastwest==0 && verthori==0) return zdcsmd_x[strip-1]-mZDCSMDCenterex;
  if(eastwest==1 && verthori==0) return mZDCSMDCenterwx-zdcsmd_x[strip-1];
  if(eastwest==0 && verthori==1) return zdcsmd_y[strip-1]/sqrt(2.)-mZDCSMDCenterey;
  if(eastwest==1 && verthori==1) return zdcsmd_y[strip-1]/sqrt(2.)-mZDCSMDCenterwy;

  return 0;
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

      double phiWgt = Weight(selN, harN, pFlowTrack);      
      float phi = pFlowTrack->Phi();
      theG *= (1. + (phiWgt/mult) * (2.* Zx * cos(order * phi) + 
				     2.* Zy * sin(order * phi) ) );            
    }
  }

  return theG;
}

//-----------------------------------------------------------------------

Double_t StFlowEvent::G_Mix(StFlowSelection* pFlowSelect, Double_t Z1x, Double_t Z1y, Double_t Z2x, Double_t Z2y) { 

  //only make sense in v1{3} calculation.
  //which means, one should call this function with StFlowSelection with harN==0 only !

  int selN     = pFlowSelect->Sel();
  int harN     = pFlowSelect->Har();
  double order = (double)(harN + 1); //should be 1 always in this func.

  bool oddHar = (harN+1) % 2;
  double etaWgt=1.;
  double ptWgt=1.;

  double mult = (double)Mult(pFlowSelect);
  Double_t theG = 1.;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {

      double detectorV1Wgt = 1.;

      if (pFlowTrack->TopologyMap().hasHitInDetector(kTpcId) ||  
	  (pFlowTrack->TopologyMap().data(0) == 0 && 
	   pFlowTrack->TopologyMap().data(1) == 0)) {
	// hits in Tpc or TopologyMap not available
	detectorV1Wgt =mV1TPCDetctWgtG_Mix[selN];
      } else if (pFlowTrack->TopologyMap().trackFtpcEast() ) {
	detectorV1Wgt =mV1FtpcEastDetctWgtG_Mix[selN];
      } else if (pFlowTrack->TopologyMap().trackFtpcWest() ) { 
	detectorV1Wgt =mV1FtpcWestDetctWgtG_Mix[selN];
      }
      
      double detectorV2Wgt = 1.;

      if (pFlowTrack->TopologyMap().hasHitInDetector(kTpcId) ||  
	  (pFlowTrack->TopologyMap().data(0) == 0 && 
	   pFlowTrack->TopologyMap().data(1) == 0)) {
	// hits in Tpc or TopologyMap not available
	detectorV2Wgt =mV2TPCDetctWgtG_Mix[selN];
      } else if (pFlowTrack->TopologyMap().trackFtpcEast() ) {
	detectorV2Wgt =mV2FtpcEastDetctWgtG_Mix[selN];
      } else if (pFlowTrack->TopologyMap().trackFtpcWest() ) { 
	detectorV2Wgt =mV2FtpcWestDetctWgtG_Mix[selN];
      }

      double phiWgt = 1.;//no need raw phi weight for cumulant method.
      float phi = pFlowTrack->Phi();
      double pt = pFlowTrack->Pt();
      double eta = pFlowTrack->Eta();

      if (oddHar) etaWgt =( (eta>0) ? (EtaAbsWgtValue(eta)) :  (-1.*EtaAbsWgtValue(eta)) );
      
      ptWgt = PtAbsWgtValue(pt);
      
      theG *= 
	(1. + (phiWgt*etaWgt*detectorV1Wgt/mult) * (2.* Z1x * cos(order * phi) + 
						    2.* Z1y * sin(order * phi) ) 
	 + (phiWgt*ptWgt*detectorV2Wgt/mult) * (2.* Z2x * cos(phi * order*2.) + 
						2.* Z2y * sin(phi * order*2.) ) );
          
    }
  }

  return theG;
}

//-------------------------------------------------------------
void StFlowEvent::SetSelections() {
  // for particles used for the event plane

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); 
       itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    double eta = (double)(pFlowTrack->Eta());
    float  Pt  = pFlowTrack->Pt();
    StTrackTopologyMap map = pFlowTrack->TopologyMap();

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
    
    if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) {
      // Tpc track or TopologyMap not available)
      
      if (mDcaGlobalTpcCuts[1] > mDcaGlobalTpcCuts[0] &&
	  (gDca < mDcaGlobalTpcCuts[0] || gDca >= mDcaGlobalTpcCuts[1])) continue;
    }
    else if (map.trackFtpcEast() || map.trackFtpcWest()) {
      // Ftpc track
      
      if (mDcaGlobalFtpcCuts[1] > mDcaGlobalFtpcCuts[0] &&
	  (gDca < mDcaGlobalFtpcCuts[0] || gDca >= mDcaGlobalFtpcCuts[1])) continue;
    }

    for (int selN = 0; selN < Flow::nSels; selN++) {
      for (int harN = 0; harN < Flow::nHars; harN++) {
	    
	  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) {
	    // Tpc track or TopologyMap not available
	    
	    // Eta
// 	    if (mEtaTpcCuts[1][harN%2][selN] > mEtaTpcCuts[0][harN%2][selN] && 
// 		(fabs(eta) < mEtaTpcCuts[0][harN%2][selN] || 
// 		 fabs(eta) >= mEtaTpcCuts[1][harN%2][selN])) continue;
	    int etaCut = harN ? 1 : 0;
	    if (mEtaTpcCuts[1][etaCut][selN] > mEtaTpcCuts[0][etaCut][selN] && 
		(fabs(eta) < mEtaTpcCuts[0][etaCut][selN] || 
		 fabs(eta) >= mEtaTpcCuts[1][etaCut][selN])) continue;
	    
	    // Pt
	    if (mPtTpcCuts[1][harN%2][selN] > mPtTpcCuts[0][harN%2][selN] && 
		(Pt < mPtTpcCuts[0][harN%2][selN] ||
		 Pt >= mPtTpcCuts[1][harN%2][selN])) continue;
	  }	  
	  else if (map.trackFtpcEast() || map.trackFtpcWest()) {
	    // Ftpc track
	    
	    // Eta
	    if (eta < 0.) {
	      if (mEtaFtpcCuts[1][harN%2][selN] > mEtaFtpcCuts[0][harN%2][selN] && 
		  (eta < mEtaFtpcCuts[0][harN%2][selN] || 
		   eta >= mEtaFtpcCuts[1][harN%2][selN])) continue;
	    }	    
	    else { // eta > 0.
	      if (mEtaFtpcCuts[3][harN%2][selN] > mEtaFtpcCuts[2][harN%2][selN] && 
		  (eta < mEtaFtpcCuts[2][harN%2][selN] || 
		   eta >= mEtaFtpcCuts[3][harN%2][selN])) continue;
	    }

	    // Pt
	    if (mPtFtpcCuts[1][harN%2][selN] > mPtFtpcCuts[0][harN%2][selN] && 
		(Pt < mPtFtpcCuts[0][harN%2][selN] ||
		 Pt >= mPtFtpcCuts[1][harN%2][selN])) continue;	
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
    Char_t pid[10] = "NA";
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
    Char_t pid[10] = "NA";

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
  // Centrality=0 is not retrieveable

  Int_t* cent = 0;
  Int_t  tracks = mMultEta; // converts UInt_t to Int_t

  if (mRunID > 8000000) { // year 7
    cent = Flow::cent200Year7;
  } else if (mRunID > 4000000) { // year 4
    if (mCenterOfMassEnergy >= 199.) {
      if (fabs(mMagneticField) >= 4.) { // year=4, Au+Au, Full Field
	cent = Flow::cent200Year4Full;
      } else { // year=4, Au+Au, Half Field
	cent = Flow::cent200Year4Half;
      }
    } else if (mCenterOfMassEnergy >60. && mCenterOfMassEnergy < 65.) { // 62 GeV
      cent = Flow::cent62;
    }
  } else if (mCenterOfMassEnergy >= 199.) {
    if (fabs(mMagneticField) >= 4.) { // year=2, Au+Au, Full Field
      cent = Flow::cent200Full;
    } else { // year=2, Au+Au, Half Field
      cent = Flow::cent200Half;
    }
  } else if (mCenterOfMassEnergy == 0.) { // year=1
    cent = Flow::cent130;
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
    cout << "#    PtWgt Saturation= " << mPtWgtSaturation << endl;
  } else {
    cout << "#    PtWgt= FALSE" << endl;
  }
  if (mEtaWgt) {
    cout << "#    EtaWgt= TRUE, also for output of PhiWgt file for 1st harmonic" << endl;
  } else {
    cout << "#    EtaWgt= FALSE" << endl;
  }
  if (mEtaSubs) {
    cout << "#    EtaSubs= TRUE" << endl;
  } else {
    cout << "#    EtaSubs= FALSE" << endl;
  }
  if (mRanSubs) {
    cout << "#    RanSubs= TRUE" << endl;
  } else {
    cout << "#    RanSubs= FALSE" << endl;
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
  cout << "# Global Dca Tpc cuts=  " << mDcaGlobalTpcCuts[0] << ", " 
       << mDcaGlobalTpcCuts[1] << endl;
  cout << "# Global Dca Ftpc cuts= " << mDcaGlobalFtpcCuts[0] << ", " 
       << mDcaGlobalFtpcCuts[1] << endl;
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < 2; j++) {
      cout << "#  selection= " << k+1 << " harmonic= " 
	   << j+1 << endl;
      cout << "#    abs(Eta) Tpc cuts= " << mEtaTpcCuts[0][j][k] << ", " 
	   << mEtaTpcCuts[1][j][k] << endl;
      cout << "#    Eta Ftpc cuts= " << mEtaFtpcCuts[0][j][k] << ", " 
	   << mEtaFtpcCuts[1][j][k] << ", " << mEtaFtpcCuts[2][j][k] << ", " 
	   << mEtaFtpcCuts[3][j][k] << endl;
      cout << "#    Pt Tpc cuts= " << mPtTpcCuts[0][j][k] << ", "
	   << mPtTpcCuts[1][j][k] << endl;
      cout << "#    Pt Ftpc cuts= " << mPtFtpcCuts[0][j][k] << ", "
	   << mPtFtpcCuts[1][j][k] << endl;
    }
  }
  cout << "#######################################################" << endl;
  
}

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.cxx,v $
// Revision 1.63  2010/09/30 19:30:26  posk
// Instead of reversing the weight for negative pseudrapidity for odd harmonics,
// it is now done only for the first harmonic.
// Recentering is now done for all harmonics.
//
// Revision 1.62  2009/11/24 19:23:01  posk
// Added reCenter option to remove acceptance correlations instead of phiWgt.
//
// Revision 1.61  2009/08/04 23:00:28  posk
// Reads year 7 MuDsts.
//
// Revision 1.60  2007/02/06 18:57:52  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.59  2006/07/06 16:56:00  posk
// Calculation of v1 for selection=2 is done with mixed harmonics.
//
// Revision 1.58  2006/02/22 19:29:14  posk
// Additions needed for the StFlowLeeYangZerosMaker
//
// Revision 1.57  2005/10/27 17:53:19  aihong
// changes made so that the cumulant method won't pick up the raw phi weight
//
// Revision 1.56  2005/02/11 23:24:29  posk
// SetCentrality works for year4.
//
// Revision 1.55  2005/02/08 20:57:36  psoren
// trigger and centrality selections were updated for all runs after run 4 to be compatible with trigger collections. Added TriggersFound() and GetFlowTriggerBitMap() functions.
//
// Revision 1.54  2004/12/17 22:33:10  aihong
// add in full Psi weight for ZDC SMD and fix a few bugs, done by Gang
//
// Revision 1.53  2004/12/17 15:50:00  aihong
// check in v1{3} code
//
// Revision 1.52  2004/12/09 23:43:35  posk
// Minor changes in code formatting.
//
// Revision 1.51  2004/12/07 23:08:11  posk
// Only odd and even phiWgt hists. If the old phiWgt file contains more than
// two harmonics, only the first two are read. Now writes only the first two.
//
// Revision 1.50  2004/12/07 17:04:44  posk
// Eliminated the very old mOnePhiWgt, which used one phiWgt histogram for flttening
// instead of four.
//
// Revision 1.49  2004/11/16 21:22:21  aihong
// removed old cumulant method
//
// Revision 1.48  2004/08/24 20:24:33  oldi
// Minor modifications to avoid compiler warnings.
// Small bug fix (didn't affect anyone yet).
//
// Revision 1.47  2004/05/31 20:09:35  oldi
// PicoDst format changed (Version 7) to hold ZDC SMD information.
// Trigger cut modified to comply with TriggerCollections.
// Centrality definition for 62 GeV data introduced.
// Minor bug fixes.
//
// Revision 1.46  2004/05/05 21:13:45  aihong
// Gang's code for ZDC-SMD added
//
// Revision 1.45  2004/03/11 17:58:40  posk
// Added Random Subs analysis method.
//
// Revision 1.44  2003/09/02 17:58:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.43  2003/07/30 22:00:39  oldi
// Eta cuts for event plane selection separated for FTPC east and west.
// PtWgtSaturation parameter introduced (default set to 2. -> no change of default behavior).
//
// Revision 1.42  2003/07/15 18:34:26  oldi
// Printout for upper FTPC pt cut for the event plane determination was showing
// upper TPC pt cut always. Changed.
//
// Revision 1.41  2003/06/18 17:00:49  posk
// Event plane cuts now only odd and even, instead of different for each harmonic.
//
// Revision 1.40  2003/05/15 06:08:41  aihong
// default PID is changed from none to NA, SetDedxPtsPart() added
//
// Revision 1.39  2003/05/02 21:09:41  posk
// Reduced the number of harmonics from 3 to 2.
//
// Revision 1.38  2003/04/01 00:27:05  posk
// Little q is now unweighted by pt or eta. Big Q is unaffected.
//
// Revision 1.37  2003/02/25 19:28:40  posk
// Changed a few unimportant default cuts.
//
// Revision 1.36  2003/01/10 16:42:09  oldi
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
// Revision 1.35  2003/01/08 19:26:46  posk
// PhiWgt hists sorted on sign of z of first and last points.
// Version 6 of pico file.
//
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
