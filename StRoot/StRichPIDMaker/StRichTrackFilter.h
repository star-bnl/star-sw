/**********************************************************
 * $Id: StRichTrackFilter.h,v 2.0 2000/08/09 16:26:22 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrackFilter.h,v $
 *  Revision 2.0  2000/08/09 16:26:22  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
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
  void setChi2(double);
  void setZVertex(double);
protected:

private:

  StRichTrack* mStRichTrack;
  
  StRichGeometryDb* myGeometryDb;
  
  double  mAngleOfIncidence;
  int     mNumberOfTPCHits;  
  double  mChiSqr;
  double  mImpactParameter;
  double  mMinNTPCHits;
  double  mPrimaryTrackSigma;

  double  mZVertex;
  StThreeVector<double>  mMomentum;
  StThreeVector<double>  mMIP;
  
  double mChi2Cut;
  double mNTPCPoints;
  double mImpactPar;
  double mMomCut;
  double mFlag;

};

#endif











