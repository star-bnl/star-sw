//###########################################################
// EMC Pico Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StEmcMicroTrack.h"

ClassImp(StEmcMicroTrack)

StEmcMicroTrack::StEmcMicroTrack()
{
}
StEmcMicroTrack::StEmcMicroTrack(StEmcMicroTrack* track):TObject()
{  
  mP         = track->getP();
  mEta       = track->getEta();
  mPhi       = track->getPhi();
  mCurvature = track->getCurvature();
  for(Int_t i=0;i<3;i++) mX[i]=track->getOrigin(i);
  mCharge    = (Char_t)track->getCharge();
  mDca       = track->getDca();
  mDcaSigned = track->getDcaSigned();
  mChi2      = (Int_t)(track->getChi2()*1000.);
  mFitPts    = (Char_t)track->getFitPts();
  mMaxPts    = (Char_t)track->getMaxPts();
  mNhits     = (Char_t)track->getNhits();
  mDedx      = track->getDedx();
  mDedxErr   = track->getDedxErr();
  mNdedxPts  = (Char_t)track->getNdedxPts();
  mTrackLength = track->getTrackLength();
  mTrackNode = track->getTrackNode();
  mFlag      = (Char_t)track->getFlag();
}
StEmcMicroTrack::~StEmcMicroTrack()
{
}
