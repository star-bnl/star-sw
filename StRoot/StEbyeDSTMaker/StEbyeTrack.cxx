/**********************************************************************
 *
 * $Id: StEbyeTrack.cxx,v 1.1.1.1 2000/08/01 13:57:55 jgreid Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 *         incorporates elements of code by
 *         Poskanzer, Snellings, & Voloshin
 *
 **********************************************************************
 *
 * Description:  This maker defines the track structure for the
 *               event-by-event DST.
 *
 **********************************************************************
 *
 * $Log: StEbyeTrack.cxx,v $
 * Revision 1.1.1.1  2000/08/01 13:57:55  jgreid
 * EbyE DST creation and access tools
 *
 *
 *********************************************************************/

#include "StEbyeTrack.h"

ClassImp(StEbyeTrack)

StEbyeTrack::StEbyeTrack(StEbyeTrack *track) : TObject() {
  mPx = track->Px();
  mPy = track->Py();
  mPz = track->Pz();

  mEta = track->Eta();
  mPhi = track->Phi();

  mBx = track->Bx();
  mBy = track->By();
  mBz = track->Bz();

  mPIDe = track->PIDe();
  mPIDpi = track->PIDpi();
  mPIDp = track->PIDp();
  mPIDk = track->PIDk();

  mChi2 = track->Chi2();

  mNFitPoints = track->NFitPoints();
  mNFoundPoints = track->NFoundPoints();
  mNMaxPoints = track->NMaxPoints();

  mDetectorID = track->DetectorID();
  mFlag = track->Flag();

  mCharge = track->Charge();
}

Float_t StEbyeTrack::Pt() { 
  return sqrt((mPx*mPx)+(mPy*mPy)); 
}

Float_t StEbyeTrack::Mt(Float_t mass) { 
  return sqrt((mPx*mPx)+(mPy*mPy)+(mass*mass)); 
}

Float_t StEbyeTrack::E(Float_t mass) { 
  return ((mPx*mPx)+(mPy*mPy)+(mPz*mPz)+(mass*mass)); 
}

Float_t StEbyeTrack::Rapidity(Float_t mass) { 
  Float_t E = this->E(mass);

  return 0.5*log((E+mPz)/(E-mPz)); 
}

Float_t StEbyeTrack::Dca() { 
  return (sqrt((mBx*mBx)+(mBy*mBy)+(mBz*mBz))); 
}

Float_t StEbyeTrack::PIDpiPlus() { 
  return ((mCharge == 1) ? mPIDpi : 0); 
}

Float_t StEbyeTrack::PIDpiMinus() { 
  return ((mCharge == -1) ? mPIDpi : 0); 
}

