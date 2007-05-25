#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StGammaTrack.h"

ClassImp(StGammaTrack);

StGammaTrack::StGammaTrack(const StMuTrack* track)
{
  mId = track->id();
  mFlag = track->flag();
  mNhits = track->nHitsFit();
  mPt = track->pt();
  mEta = track->eta();
  mPhi = track->phi();
  mdEdx = track->dEdx() / keV; 
  mType = track->type();
}
StGammaTrack::StGammaTrack()
{
  mId=0;
  mFlag=-1;
  mNhits=0;
  mPt=0.;
  mEta=-10.;
  mPhi=0.;
  mdEdx=0.;
}
