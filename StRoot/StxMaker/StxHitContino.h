// $Id: StxHitContino.h,v 2.2 2009/08/04 18:55:12 fisyak Exp $
// $Log: StxHitContino.h,v $
// Revision 2.2  2009/08/04 18:55:12  fisyak
// Capitilize method names
//
// Revision 2.1  2009/07/19 20:13:48  fisyak
// remove abstract classes
//
#ifndef  __StxHitContino_h__
#define __StxHitContino_h__
// $Id: StxHitContino.h,v 2.2 2009/08/04 18:55:12 fisyak Exp $
// $Log: StxHitContino.h,v $
// Revision 2.2  2009/08/04 18:55:12  fisyak
// Capitilize method names
//
// Revision 2.1  2009/07/19 20:13:48  fisyak
// remove abstract classes
//
/*! \class StxHitContino
  Axiliary class for StxKalmanTrackNode only. Small container
  of about 3 best hits for this node. Used for refit.
  \author Victor Perev
*/
#include "StxHit.h"
class StxHitContino {
public:
  StxHitContino()			{Reset();}
  void     Reset(); 		
  StxHit  *Hit (Int_t idx) const	{return mHits[idx];}
  Int_t    NHits () const;		
  Double_t Chi2 (Int_t idx=0) const	{return mChi2[idx];}
  Double_t Detr (Int_t idx=0) const	{return mDetr[idx];}
  void     Add (StxHit *hit,Double_t chi2,Double_t detr=1.);
  void     Print (const char* opt="") const;
private:
  enum {kMaxSize=10};
  StxHit   *mHits[kMaxSize+1];
  Double_t  mChi2[kMaxSize+1];
  Double_t  mDetr[kMaxSize+1];
};
#endif
