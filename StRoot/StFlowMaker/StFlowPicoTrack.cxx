////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoTrack.cxx,v 1.11 2003/01/08 19:26:51 posk Exp $
//
// Author: Raimond Snellings, March 2000
//
// Description:  A persistent Flow Pico DST
//
////////////////////////////////////////////////////////////////////////

#include "StFlowPicoTrack.h"
#include <math.h>
// Needed for Solaris, which does not have fabs():
#define ABS(x) (x>0?x:-x)

ClassImp(StFlowPicoTrack)

//-----------------------------------------------------------------------

StFlowPicoTrack::StFlowPicoTrack(StFlowPicoTrack *track) : TObject() {

  Float_t maxInt = 32.;
  Float_t pid;

  mPt          = track->Pt();
  mPtGlobal    = track->PtGlobal();
  mEta         = track->Eta();
  mEtaGlobal   = track->EtaGlobal();
  mDedx        = track->Dedx();
  mPhi         = track->Phi();
  mPhiGlobal   = track->PhiGlobal();
  mCharge      = track->Charge();
  mDca         = track->Dca();
  mDcaSigned   = track->DcaSigned();
  mDcaGlobal   = track->DcaGlobal();
  mZFirstPoint = track->ZFirstPoint();
  mZLastPoint  = track->ZLastPoint();
  mChi2        = track->Chi2();
  mFitPts      = track->FitPts();
  mMaxPts      = track->MaxPts();
  mNhits       = track->Nhits();
  mNdedxPts    = track->NdedxPts();
  mDcaGlobalX  = (Float_t)track->DcaGlobalX();
  mDcaGlobalY  = (Float_t)track->DcaGlobalY();
  mDcaGlobalZ  = (Float_t)track->DcaGlobalZ();
  mTrackLength = track->TrackLength();
  mMostLikelihoodPID  = track->MostLikelihoodPID();
  mMostLikelihoodProb = track->MostLikelihoodProb();
  mExtrapTag = track->ExtrapTag();
  mElectronPositronProb = track->ElectronPositronProb();
  mPionPlusMinusProb = track->PionPlusMinusProb();
  mKaonPlusMinusProb = track->KaonPlusMinusProb();
  mProtonPbarProb = track->ProtonPbarProb();
  mTopologyMap0 = track->TopologyMap0();
  mTopologyMap1 = track->TopologyMap1();

  pid = track->PidPion();
  if (ABS(pid) > maxInt) pid = maxInt; mPidPion     = (Int_t)(pid*1000.); 
  pid = track->PidProton();
  if (ABS(pid) > maxInt) pid = maxInt; mPidProton   = (Int_t)(pid*1000.); 
  pid = track->PidKaon();
  if (ABS(pid) > maxInt) pid = maxInt; mPidKaon     = (Int_t)(pid*1000.); 
  pid = track->PidDeuteron();
  if (ABS(pid) > maxInt) pid = maxInt; mPidDeuteron = (Int_t)(pid*1000.); 
  pid = track->PidElectron();
  if (ABS(pid) > maxInt) pid = maxInt; mPidElectron = (Int_t)(pid*1000.); 

}

//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoTrack.cxx,v $
// Revision 1.11  2003/01/08 19:26:51  posk
// PhiWgt hists sorted on sign of z of first and last points.
// Version 6 of pico file.
//
// Revision 1.10  2001/12/18 19:22:35  posk
// "proton" and "antiproton" changed to "pr+" and "pr-".
// Compiles on Solaris.
//
// Revision 1.9  2001/12/12 21:36:20  posk
// For Solaris compatability, added: #include <math.h>
//
// Revision 1.8  2001/07/27 01:26:37  snelling
// Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
// Revision 1.7  2001/07/24 22:29:37  snelling
// First attempt to get a standard root pico file again, added variables
//
// Revision 1.6  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.5  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
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
