/***************************************************************************
 *
 * $Id: StHbtTrack.cc,v 1.2 2000/04/04 12:49:05 laue Exp $
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
  mNSigmaElectron = t.mNSigmaElectron;
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
  mMap[0] = t.mMap[0];
  mMap[1] = t.mMap[1];
  mTrackId = t.mTrackId;
};

