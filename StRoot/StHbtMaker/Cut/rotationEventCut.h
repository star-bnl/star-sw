/***************************************************************************
 *
 * $Id: rotationEventCut.h,v 1.3 2001/12/05 15:13:22 laue Exp $
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
 * $Log: rotationEventCut.h,v $
 * Revision 1.3  2001/12/05 15:13:22  laue
 * rotationEventCut.h: cut on l3 trigger algorithm
 * franksXiCut.cxx: updates
 *
 * Revision 1.2  2001/06/21 19:09:44  laue
 * updated to match changed base classes
 *
 * Revision 1.1  2000/06/14 18:22:33  laue
 * New event cut
 *
 * Revision 1.1  2000/05/25 21:47:28  laue
 * new event cut which can be used to rotate an event around the z-axis
 *
 *
 **************************************************************************/
#ifndef rotationEventCut_hh
#define rotationEventCut_hh


#include "StHbtMaker/Base/StHbtEventCut.h"
class HepRandom;
class HepJamesRandom;
class RandGauss;

class rotationEventCut : public StHbtEventCut {
public:

  rotationEventCut();
  rotationEventCut(rotationEventCut&);
  //~rotationEventCut();

  void SetEventRefMult(const int& lo,const int& hi);
  void SetEventMult(const int& lo,const int& hi);
  void SetEventMultGood(const int& lo,const int& hi);
  void SetNumberOfTracks(const int& lo,const int& hi);
  void SetNumberOfV0s(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);
  void SetReactionPlaneError(const float& lo, const float& hi);
  void SetL3TriggerAlgorithm(const unsigned int&);
  void SetSmear(const float& s=0);
  void RotationOn();
  void RotationOff();
  void RandomOn();
  void RandomOff();
  long NEventsPassed();
  long NEventsFailed();


  virtual StHbtString Report();
  virtual bool Pass(const StHbtEvent*);

  rotationEventCut* Clone();

private:   // here are the quantities I want to cut on...
  StHbtTrackCollection* ChargeList(StHbtTrackCollection*, const unsigned short);
  StHbtTrackCollection* RemoveList(StHbtTrackCollection*, const float, const float);

  bool  mRotation;
  bool  mRandom;
  float mSmear;
  int   mEventRefMult[2];      // range of multiplicity
  int   mEventMult[2];      // range of multiplicity
  int   mEventMultGood[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex
  int mNumberOfTracks[2];
  int mNumberOfV0s[2];
  float mReactionPlaneError[2];
  unsigned int mL3TriggerAlgorithm;

  long mNEventsPassed;
  long mNEventsFailed;
  HepJamesRandom* engine; //!
  RandGauss* gauss; //!

#ifdef __ROOT__
  ClassDef(rotationEventCut, 1)
#endif

};

inline void rotationEventCut::SetEventRefMult(const int& lo, const int& hi){mEventRefMult[0]=lo; mEventRefMult[1]=hi;}
inline void rotationEventCut::SetEventMult(const int& lo, const int& hi){mEventMult[0]=lo; mEventMult[1]=hi;}
inline void rotationEventCut::SetEventMultGood(const int& lo, const int& hi){mEventMultGood[0]=lo; mEventMultGood[1]=hi;}
inline void rotationEventCut::SetNumberOfTracks(const int& lo, const int& hi){mNumberOfTracks[0]=lo; mNumberOfTracks[1]=hi;}
inline void rotationEventCut::SetNumberOfV0s(const int& lo, const int& hi){mNumberOfV0s[0]=lo; mNumberOfV0s[1]=hi;}
inline void rotationEventCut::SetVertZPos(const float& lo, const float& hi){mVertZPos[0]=lo; mVertZPos[1]=hi;}
inline void rotationEventCut::SetReactionPlaneError(const float& lo, const float& hi){mReactionPlaneError[0]=lo; mReactionPlaneError[1]=hi;}
inline void rotationEventCut::SetL3TriggerAlgorithm(const unsigned int& l3) {mL3TriggerAlgorithm=l3;}
inline void rotationEventCut::SetSmear(const float& s){ mSmear = s;}
inline void rotationEventCut::RotationOn() { mRotation = true; }
inline void rotationEventCut::RotationOff() { mRotation = false; }
inline void rotationEventCut::RandomOn() { mRandom = true; }
inline void rotationEventCut::RandomOff() { mRandom = false; }
inline long  rotationEventCut::NEventsPassed() {return mNEventsPassed;}
inline long  rotationEventCut::NEventsFailed() {return mNEventsFailed;}
inline rotationEventCut* rotationEventCut::Clone() { rotationEventCut* c = new rotationEventCut(*this); return c;}
inline rotationEventCut::rotationEventCut(rotationEventCut& c) : StHbtEventCut(c) {
  mRotation = c.mRotation;
  mRandom = c.mRandom;
  mSmear = c.mSmear;
  mEventMult[0] = c.mEventMult[0];
  mEventMult[1] = c.mEventMult[1];
  mEventRefMult[0] = c.mEventRefMult[0];
  mEventRefMult[1] = c.mEventRefMult[1];
  mEventMultGood[0] = c.mEventMultGood[0];
  mEventMultGood[1] = c.mEventMultGood[1];
  mVertZPos[0] = c.mVertZPos[0];
  mVertZPos[1] = c.mVertZPos[1];
  mNumberOfTracks[0] = c.mNumberOfTracks[0];
  mNumberOfTracks[1] = c.mNumberOfTracks[1];
  mNumberOfV0s[0] = c.mNumberOfV0s[0];
  mNumberOfV0s[1] = c.mNumberOfV0s[1];
  mReactionPlaneError[0] = c.mReactionPlaneError[0];
  mReactionPlaneError[1] = c.mReactionPlaneError[1];
  mL3TriggerAlgorithm = c.mL3TriggerAlgorithm;
  mNEventsPassed = 0;
  mNEventsFailed = 0;
}


#endif
