/**********************************************************
 * $Id: StRichRingHit.cxx,v 2.2 2000/10/19 01:13:23 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingHit.cxx,v $
 *  Revision 2.2  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  added normal distance sigma cut on hits, quartz and radiator pathlengths
 *  for individual photons, modified minimization routine to correct boundary
 *  problems
 *
 *  Revision 2.1  2000/09/29 01:35:37  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/22 15:14:44  horsley
 *  modified StRichRingHit, StRichTDrawableRings to comply with sun compiler
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/

#include "StRichRingHit.h"
#include "StRichHit.h"


StRichRingHit::StRichRingHit(StRichHit* hit, double angle, double dist, double sigma, 
			     double radPath, double quaPath) {
  mHit = hit;
  mAngle = angle;
  mDist = dist;
  mNSigma = sigma;
  mPathInRadiator = radPath;
  mPathInQuartz = quaPath;
}

StRichRingHit::~StRichRingHit() {}

StRichHit* StRichRingHit::getHit() {
  return mHit;
}


double StRichRingHit::getAngle() {
  return mAngle;
}

double StRichRingHit::getDist() {
  return mDist;
}


double StRichRingHit::getNSigma() {
  return mNSigma;
}



double StRichRingHit::getMeanPathInQuartz() {
  return mPathInQuartz;
}

double StRichRingHit::getMeanPathInRadiator() {
  return mPathInRadiator;
}

