/***************************************************************************
 *
 * $Id: rotateToReactionPlaneEventCut.h,v 1.1 2000/05/25 21:47:28 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex and rotates the event around the z-axis          
 *
 ***************************************************************************
 *
 * $Log: rotateToReactionPlaneEventCut.h,v $
 * Revision 1.1  2000/05/25 21:47:28  laue
 * new event cut which can be used to rotate an event around the z-axis
 *
 *
 **************************************************************************/

#ifndef rotateToReactionPlaneEventCut_hh
#define rotateToReactionPlaneEventCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtEventCut.h"
class HepRandom;
class HepJamesRandom;
class RandGauss;

class rotateToReactionPlaneEventCut : public StHbtEventCut {
public:

  rotateToReactionPlaneEventCut();
  rotateToReactionPlaneEventCut(rotateToReactionPlaneEventCut&);
  //~rotateToReactionPlaneEventCut();

  void SetEventMult(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);
  void SetSmear(const float& s=0);
  void RotationOn();
  void RotationOff();
  long NEventsPassed();
  long NEventsFailed();

  virtual StHbtString Report();
  virtual bool Pass(const StHbtEvent*);

  rotateToReactionPlaneEventCut* Clone();

private:   // here are the quantities I want to cut on...

  bool  mRotation;
  float mSmear;
  int   mEventMult[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex

  long mNEventsPassed;
  long mNEventsFailed;
  HepJamesRandom* engine; //!
  RandGauss* gauss; //!

#ifdef __ROOT__
  ClassDef(rotateToReactionPlaneEventCut, 1)
#endif

};

inline void rotateToReactionPlaneEventCut::SetEventMult(const int& lo, const int& hi){mEventMult[0]=lo; mEventMult[1]=hi;}
inline void rotateToReactionPlaneEventCut::SetVertZPos(const float& lo, const float& hi){mVertZPos[0]=lo; mVertZPos[1]=hi;}
inline void rotateToReactionPlaneEventCut::SetSmear(const float& s){ mSmear = s;}
inline void rotateToReactionPlaneEventCut::RotationOn() { mRotation = true; }
inline void rotateToReactionPlaneEventCut::RotationOff() { mRotation = false; }
inline long  rotateToReactionPlaneEventCut::NEventsPassed() {return mNEventsPassed;}
inline long  rotateToReactionPlaneEventCut::NEventsFailed() {return mNEventsFailed;}
inline rotateToReactionPlaneEventCut* rotateToReactionPlaneEventCut::Clone() { rotateToReactionPlaneEventCut* c = new rotateToReactionPlaneEventCut(*this); return c;}
inline rotateToReactionPlaneEventCut::rotateToReactionPlaneEventCut(rotateToReactionPlaneEventCut& c) : StHbtEventCut(c) {
  mRotation = c.mRotation;
  mSmear = c.mSmear;
  mEventMult[0] = c.mEventMult[0];
  mEventMult[1] = c.mEventMult[1];
  mVertZPos[0] = c.mVertZPos[0];
  mVertZPos[1] = c.mVertZPos[1];
  mNEventsPassed = 0;
  mNEventsFailed = 0;
}


#endif
