/**********************************************************
 * $Id: StRichTrackFilter.h,v 2.1 2000/09/29 01:35:38 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrackFilter.h,v $
 *  Revision 2.1  2000/09/29 01:35:38  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/

#ifndef STRICHTRACKFILTER_H
#define STRICHTRACKFILTER_H

#include "StThreeVector.hh"
#include "StThreeVectorF.hh"

class StRichTrack;
class StRichGeometryDb;

class StRichTrackFilter {

public:

  StRichTrackFilter();
  ~StRichTrackFilter();
  bool trackAcceptable();
  bool fromPrimaryVertex(int&);  
  void setTrack(StRichTrack* );
  void setMomentum(double);
  void setImpactParameter(double);
  void setNTPCPoints(int);
  void setVertex(StThreeVectorF&);

protected:

private:

  StRichTrack* mStRichTrack;
  
  StRichGeometryDb* myGeometryDb;
  
  double  mAngleOfIncidence;
  int     mNumberOfTPCFitPoints;
  double  mDCA;
  double  mEtaCut;
  double  mMinNTPCFitPoints;
  StThreeVectorF  mMomentum;
  StThreeVectorF  mMIP;
  StThreeVectorF  mVertex;
  double mFlag;

};

#endif











