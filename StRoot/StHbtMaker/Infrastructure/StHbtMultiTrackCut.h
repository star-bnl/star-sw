/***************************************************************************
 *
 * $Id: 
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * This is a class that holds a collection of StHbtTrackCuts
 * 
 ***************************************************************************
 *
 **************************************************************************/


#ifndef StHbtMultiTrackCut_hh
#define StHbtMultiTrackCut_hh

#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"


#include "StHbtMaker/Infrastructure/StHbtTrackCutCollection.hh"


class StHbtMultiTrackCut : public StHbtTrackCut {
 private:
  StHbtTrackCutCollection* mCutCollection;
 public:

  StHbtMultiTrackCut();
  StHbtMultiTrackCut(const StHbtMultiTrackCut&);                // copy constructor
  virtual ~StHbtMultiTrackCut();

  virtual StHbtString Report();             // user-written method to return string describing cuts
  virtual bool Pass(const StHbtTrack* track);       // true if passes, false if not

  virtual void AddTrackCut(StHbtTrackCut*);
  virtual void EventBegin(const StHbtEvent*);
  virtual void EventEnd(const StHbtEvent*);

  StHbtParticleType Type(){return hbtTrack;}

  virtual StHbtMultiTrackCut* Clone();

#ifdef __ROOT__
  ClassDef(StHbtMultiTrackCut, 0)
#endif
};


#endif
