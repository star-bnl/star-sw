/***************************************************************************
 *
 * $Id: mikesEventCut.h,v 1.3 1999/10/15 01:57:04 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex           
 *
 ***************************************************************************
 *
 * $Log: mikesEventCut.h,v $
 * Revision 1.3  1999/10/15 01:57:04  lisa
 * Important enhancement of StHbtMaker - implement Franks CutMonitors
 * ----------------------------------------------------------
 * This means 3 new files in Infrastructure area (CutMonitor),
 * several specific CutMonitor classes in the Cut area
 * and a new base class in the Base area (StHbtCutMonitor).
 * This means also changing all Cut Base class header files from .hh to .h
 * so we have access to CutMonitor methods from Cint command line.
 * This last means
 * 1) files which include these header files are slightly modified
 * 2) a side benefit: the TrackCuts and V0Cuts no longer need
 * a SetMass() implementation in each Cut class, which was stupid.
 * Also:
 * -----
 * Include Franks StHbtAssociationReader
 * ** None of these changes should affect any user **
 *
 * Revision 1.2  1999/07/06 22:33:21  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef mikesEventCut_hh
#define mikesEventCut_hh

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StHbtMaker/Base/StHbtEventCut.h"

class mikesEventCut : public StHbtEventCut {

public:

  mikesEventCut();
  //~mikesEventCut();

  void SetEventMult(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);
  int NEventsPassed();
  int NEventsFailed();

  virtual StHbtString Report();

  virtual bool Pass(const StHbtEvent*);

private:   // here are the quantities I want to cut on...

  int mEventMult[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex

  long mNEventsPassed;
  long mNEventsFailed;

  ClassDef(mikesEventCut, 1)

};

inline void mikesEventCut::SetEventMult(const int& lo, const int& hi){mEventMult[0]=lo; mEventMult[1]=hi;}
inline void mikesEventCut::SetVertZPos(const float& lo, const float& hi){mVertZPos[0]=lo; mVertZPos[1]=hi;}
inline int  mikesEventCut::NEventsPassed() {return mNEventsPassed;}
inline int  mikesEventCut::NEventsFailed() {return mNEventsFailed;}


#endif
