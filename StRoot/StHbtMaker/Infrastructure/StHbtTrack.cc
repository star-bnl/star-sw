/***************************************************************************
 *
 * $Id: StHbtTrack.cc,v 1.1 2000/03/27 17:34:17 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * Implementation of methods
 *
 ****************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtTrack.hh" 

StHbtTrack::StHbtTrack(const StHbtTrack& t) { // copy constructor
  mCharge = t.mCharge;
  mNHits = t.mNHits;
  mNHitsPoss = t.mNHitsPoss; 
  mNSigmaPion = t.mNSigmaPion;
  mNSigmaKaon = t.mNSigmaKaon;
  mNSigmaProton = t.mNSigmaProton;
  mdEdx = t.mdEdx;
  mDCAxy = t.mDCAxy;
  mDCAz = t.mDCAz; 
  mChiSqXY = t.mChiSqXY;
  mChiSqZ = t.mChiSqZ;
  mP = t.mP;
  mPt = t.mPt;
  mHelix = t.mHelix;
};

