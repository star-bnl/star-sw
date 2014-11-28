/***************************************************************************
 *
 * $Id: mikesKinkCut.h,v 1.1 2001/06/01 19:23:54 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: This is an example of a StHbtKinkCut, which cuts on StHbtKinks
 *     in the StHbtEvent.  If a StHbtKink passes the cut, then it becomes a
 *     StHbtParticle to be used in the StHbtAnalysis
 *
 ***************************************************************************
 *
 * $Log: mikesKinkCut.h,v $
 * Revision 1.1  2001/06/01 19:23:54  lisa
 * new example class Cut/mikesKinkCut
 *
 *
 **************************************************************************/

#ifndef mikesKinkCut_hh
#define mikesKinkCut_hh


#include "StHbtMaker/Base/StHbtKinkCut.h"

class mikesKinkCut : public StHbtKinkCut {

public:

  mikesKinkCut();
  mikesKinkCut(mikesKinkCut&);
  //~mikesKinkCut();

  void SetDcaParentDaughter(const float& lo,const float& hi);
  void SetDcaDaughterPrimaryVertex(const float& lo,const float& hi);
  void SetDcaParentPrimaryVertex(const float& lo,const float& hi);
  void SetHitDistanceParentDaughter(const float& lo,const float& hi);
  void SetHitDistanceParentVertex(const float& lo,const float& hi);
  void SetDecayAngle(const float& lo,const float& hi);
  void SetDecayAngleCM(const float& lo,const float& hi);

  int NKinksPassed();
  int NKinksFailed();

  virtual StHbtString Report();
  virtual bool Pass(const StHbtKink*);

  mikesKinkCut* Clone();

private:   // here are the quantities I want to cut on...

  float        mDcaParentDaughter[2];
  float        mDcaDaughterPrimaryVertex[2];
  float        mDcaParentPrimaryVertex[2];
  float        mHitDistanceParentDaughter[2];
  float        mHitDistanceParentVertex[2];
  float        mDecayAngle[2];
  float        mDecayAngleCM[2];


  long mNKinksPassed;
  long mNKinksFailed;

#ifdef __ROOT__
  ClassDef(mikesKinkCut, 1)
#endif

};


inline int  mikesKinkCut::NKinksPassed() {return mNKinksPassed;}
inline int  mikesKinkCut::NKinksFailed() {return mNKinksFailed;}
inline mikesKinkCut* mikesKinkCut::Clone() { mikesKinkCut* c = new mikesKinkCut(*this); return c;}
inline mikesKinkCut::mikesKinkCut(mikesKinkCut& c) : StHbtKinkCut(c) {

  mDcaParentDaughter[0]        = c.mDcaParentDaughter[0];
  mDcaDaughterPrimaryVertex[0] = c.mDcaDaughterPrimaryVertex[0];
  mDcaParentPrimaryVertex[0]   = c.mDcaParentPrimaryVertex[0];
  mHitDistanceParentDaughter[0]= c.mHitDistanceParentDaughter[0];
  mHitDistanceParentVertex[0]  = c.mHitDistanceParentVertex[0];
  mDecayAngle[0]               = c.mDecayAngle[0];
  mDecayAngleCM[0]             = c.mDecayAngleCM[0];

  mDcaParentDaughter[1]        = c.mDcaParentDaughter[1];
  mDcaDaughterPrimaryVertex[1] = c.mDcaDaughterPrimaryVertex[1];
  mDcaParentPrimaryVertex[1]   = c.mDcaParentPrimaryVertex[1];
  mHitDistanceParentDaughter[1]= c.mHitDistanceParentDaughter[1];
  mHitDistanceParentVertex[1]  = c.mHitDistanceParentVertex[1];
  mDecayAngle[1]               = c.mDecayAngle[1];
  mDecayAngleCM[1]             = c.mDecayAngleCM[1];
  mNKinksPassed = 0;
  mNKinksFailed = 0;

}


inline void mikesKinkCut::SetDcaParentDaughter(const float& lo,const float& hi){mDcaParentDaughter[0]=lo; mDcaParentDaughter[1]=hi;}
inline void mikesKinkCut::SetDcaDaughterPrimaryVertex(const float& lo,const float& hi){mDcaDaughterPrimaryVertex[0]=lo;mDcaDaughterPrimaryVertex[1]=hi;}
inline void mikesKinkCut::SetDcaParentPrimaryVertex(const float& lo,const float& hi){mDcaParentPrimaryVertex[0]=lo;mDcaParentPrimaryVertex[1]=hi;}
inline void mikesKinkCut::SetHitDistanceParentDaughter(const float& lo,const float& hi){mHitDistanceParentDaughter[0]=lo;mHitDistanceParentDaughter[1]=hi;}
inline void mikesKinkCut::SetHitDistanceParentVertex(const float& lo,const float& hi){mHitDistanceParentVertex[0]=lo;mHitDistanceParentVertex[1]=hi;}
inline void mikesKinkCut::SetDecayAngle(const float& lo,const float& hi){mDecayAngle[0]=lo;mDecayAngle[1]=hi;}
inline void mikesKinkCut::SetDecayAngleCM(const float& lo,const float& hi){mDecayAngleCM[0]=lo;mDecayAngleCM[1]=hi;}


#endif
