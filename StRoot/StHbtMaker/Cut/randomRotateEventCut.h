/***************************************************************************
 *
 * $Id: randomRotateEventCut.h,v 1.1 2000/05/25 21:47:28 laue Exp $
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
 * $Log: randomRotateEventCut.h,v $
 * Revision 1.1  2000/05/25 21:47:28  laue
 * new event cut which can be used to rotate an event around the z-axis
 *
 *
 **************************************************************************/

#ifndef randomRotateEventCut_hh
#define randomRotateEventCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtEventCut.h"
class HepRandom;

class randomRotateEventCut : public StHbtEventCut {

public:

  randomRotateEventCut();
  randomRotateEventCut(randomRotateEventCut&);
  //~randomRotateEventCut();

  void SetEventMult(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);
  void RotationOn();
  void RotationOff();
  long NEventsPassed();
  long NEventsFailed();

  virtual StHbtString Report();
  virtual bool Pass(const StHbtEvent*);

  randomRotateEventCut* Clone();

private:   // here are the quantities I want to cut on...

  bool  mRotation;
  int mEventMult[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex
  long mNEventsPassed;
  long mNEventsFailed;
  HepRandom* engine; //!

#ifdef __ROOT__
  ClassDef(randomRotateEventCut, 1)
#endif

};

inline void randomRotateEventCut::SetEventMult(const int& lo, const int& hi){mEventMult[0]=lo; mEventMult[1]=hi;}
inline void randomRotateEventCut::SetVertZPos(const float& lo, const float& hi){mVertZPos[0]=lo; mVertZPos[1]=hi;}
inline void randomRotateEventCut::RotationOn() { mRotation = true; }
inline void randomRotateEventCut::RotationOff() { mRotation = false; }
inline long  randomRotateEventCut::NEventsPassed() {return mNEventsPassed;}
inline long  randomRotateEventCut::NEventsFailed() {return mNEventsFailed;}
inline randomRotateEventCut* randomRotateEventCut::Clone() { randomRotateEventCut* c = new randomRotateEventCut(*this); return c;}
inline randomRotateEventCut::randomRotateEventCut(randomRotateEventCut& c) : StHbtEventCut(c) {
  mRotation = c.mRotation;
  mEventMult[0] = c.mEventMult[0];
  mEventMult[1] = c.mEventMult[1];
  mVertZPos[0] = c.mVertZPos[0];
  mVertZPos[1] = c.mVertZPos[1];
  mNEventsPassed = 0;
  mNEventsFailed = 0;
}


#endif
