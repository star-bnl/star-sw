#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StGammaTrack.h"

ClassImp(StGammaTrack);

StGammaTrack::StGammaTrack()
{
  mId = 0;
  mFlag = -1;
  mCharge = 0;
  mNhits = 0;
  mdEdx = 0;
}

StGammaTrack::StGammaTrack(StMuTrack* track)
{
  mId = track->id();
  mFlag = track->flag();
  mCharge = track->charge();
  mNhits = track->nHitsFit();
  mdEdx = track->dEdx() / keV; 
  mType = track->type();
  mMomentum = track->momentum().xyz();
  mDca = track->dcaGlobal().xyz();
  mHelix = track->helix();
  mOuterHelix = track->outerHelix();
}

//
// See $STAR/StRoot/StEmcUtil/projection/StEmcPosition.h
//
TVector3 StGammaTrack::positionAtRadius(double radius) const throw(Exception)
{
  static const pair<double, double> VALUE(999999999., 999999999.); // No solution

  if (mOuterHelix.origin().perp() > radius) throw Exception();
  pair<double, double> ss = mOuterHelix.pathLength(radius);
  if (!finite(ss.first) || !finite(ss.second)) throw Exception();
  if (ss == VALUE) throw Exception();

  double s = 0;

  if (ss.first > 0 && ss.second > 0)
    s = ss.first;
  else if (ss.first >= 0 && ss.second < 0)
    s = ss.first;
  else if (ss.first < 0 && ss.second >= 0)
    s = ss.second;
  else
    throw Exception();

  return mOuterHelix.at(s).xyz();
}

TVector3 StGammaTrack::positionAtZ(double z) const throw(Exception)
{
  if (fabs(mOuterHelix.origin().z()) > fabs(z)) throw Exception();
  StThreeVector<double> r(0,0,z);
  StThreeVector<double> n(0,0,1);
  double s = mOuterHelix.pathLength(r,n);
  if (!finite(s)) throw Exception();
  if (s == StHelix::NoSolution) throw Exception();
  if (s < 0) throw Exception();
  return mOuterHelix.at(s).xyz();
}
