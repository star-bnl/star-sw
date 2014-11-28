// $Id: StiHitContino.h,v 2.2 2009/08/04 18:55:12 fisyak Exp $
// $Log: StiHitContino.h,v $
// Revision 2.2  2009/08/04 18:55:12  fisyak
// Capitilize method names
//
// Revision 2.1  2009/07/19 20:13:48  fisyak
// remove abstract classes
//
#ifndef  __StiHitContino_h__
#define __StiHitContino_h__
// $Id: StiHitContino.h,v 2.2 2009/08/04 18:55:12 fisyak Exp $
// $Log: StiHitContino.h,v $
// Revision 2.2  2009/08/04 18:55:12  fisyak
// Capitilize method names
//
// Revision 2.1  2009/07/19 20:13:48  fisyak
// remove abstract classes
//
/*! \class StiHitContino
  Axiliary class for StiKalmanTrackNode only. Small container
  of about 3 best hits for this node. Used for refit.
  \author Victor Perev
*/
#include "StiHit.h"
class StiHitContino {
public:
  StiHitContino()			{Reset();}
  void     Reset(); 		
  StiHit  *Hit (Int_t idx) const	{return mHits[idx];}
  Int_t    NHits () const;		
  Double_t Chi2 (Int_t idx=0) const	{return mChi2[idx];}
  Double_t Detr (Int_t idx=0) const	{return mDetr[idx];}
  void     Add (StiHit *hit,Double_t chi2,Double_t detr=1.);
  void     Print (const char* opt="") const;
private:
  enum {kMaxSize=10};
  StiHit   *mHits[kMaxSize+1];
  Double_t  mChi2[kMaxSize+1];
  Double_t  mDetr[kMaxSize+1];
};
#endif
