////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoTrack.cxx,v 1.4 2000/10/12 22:46:39 snelling Exp $
//
// Author: Raimond Snellings, March 2000
//
// Description:  A persistent Flow Pico DST
//
////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoTrack.cxx,v $
// Revision 1.4  2000/10/12 22:46:39  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.3  2000/09/15 22:51:32  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.2  2000/09/05 17:57:12  snelling
// Solaris needs math.h for fabs
//
// Revision 1.1  2000/09/05 16:11:36  snelling
// Added global DCA, electron and positron
//
// 
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <math.h>
#include "StFlowPicoTrack.h"

ClassImp(StFlowPicoTrack)

//-----------------------------------------------------------------------

StFlowPicoTrack::StFlowPicoTrack(StFlowPicoTrack *track) : TObject() {

  Float_t maxInt = 32.;
  Float_t pid;

  mPt        = track->Pt();
  mPtGlobal  = track->PtGlobal();
  mEta       = track->Eta();
  mEtaGlobal = track->EtaGlobal();
  mDedx      = track->Dedx();
  mPhi       = track->Phi();
  mPhiGlobal = track->PhiGlobal();
  mCharge    = track->Charge();
  mDca       = track->Dca();
  mDcaGlobal = track->DcaGlobal();
  mChi2      = track->Chi2();
  mFitPts    = track->FitPts();
  mMaxPts    = track->MaxPts();
  mMostLikelihoodPID  = track->MostLikelihoodPID();
  mMostLikelihoodProb = track->MostLikelihoodProb();
  mExtrapTag = track->ExtrapTag();

  pid = track->PidPion();
  if (fabs(pid) > maxInt) pid = maxInt; mPidPion     = (Int_t)(pid*1000.); 
  pid = track->PidProton();
  if (fabs(pid) > maxInt) pid = maxInt; mPidProton   = (Int_t)(pid*1000.); 
  pid = track->PidKaon();
  if (fabs(pid) > maxInt) pid = maxInt; mPidKaon     = (Int_t)(pid*1000.); 
  pid = track->PidDeuteron();
  if (fabs(pid) > maxInt) pid = maxInt; mPidDeuteron = (Int_t)(pid*1000.); 
  pid = track->PidElectron();
  if (fabs(pid) > maxInt) pid = maxInt; mPidElectron = (Int_t)(pid*1000.); 
}
