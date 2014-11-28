/***************************************************************************
 *
 * $Id: helensEventCut.h,v 1.1 2000/10/09 21:56:15 laue Exp $
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
 * $Log: helensEventCut.h,v $
 * Revision 1.1  2000/10/09 21:56:15  laue
 * Helens new cuts
 *
 * Revision 1.5  2000/03/23 22:57:28  laue
 * Clone() function implemented
 *
 * Revision 1.4  2000/01/25 17:35:02  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
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

#ifndef helensEventCut_hh
#define helensEventCut_hh

#include "StHbtMaker/Base/StHbtEventCut.h"
#include <list>

class StHbtTrkV0Match;

#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif


#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtTrkV0Match*, allocator<StHbtTrkV0Match*> >            StHbtTrkV0MatchCollection;
typedef list<StHbtTrkV0Match*, allocator<StHbTrkV0Match*> >::iterator  StHbtTrkV0Iterator;
#else
typedef list<StHbtTrkV0Match*>            StHbtTrkV0MatchCollection;
typedef list<StHbtTrkV0Match*>::iterator  StHbtTrkV0Iterator;
#endif


class StHbtTrkV0Match{

 public:
  StHbtTrkV0Match(){ /* no-op*/};
  StHbtTrkV0Match( const StHbtTrkV0Match&); //Copy Constructor		
  ~StHbtTrkV0Match(){/* no-op */};
  
  int TrkId() {return mTrkId;};
  int V0Id()  {return mV0Id;}
  float dEdx(){return mdEdx;} ;
  int Used()  {return mUsed;};

  void SetTrkId( int i) {mTrkId=i;};
  void SetV0Id( int i)  {mV0Id=i;};
  void SetdEdx( float x) {mdEdx=x;};
  void SetUsed( int i) {mUsed=i;};

 protected:

  int mTrkId;
  int mV0Id;
  float mdEdx;
  int mUsed;
};


class helensEventCut : public StHbtEventCut {

public:

  helensEventCut();
  helensEventCut(helensEventCut&);

  void SetEventMult(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);
  void SetV0Mult(const int& lo,const int& hi);
  int NEventsPassed();
  int NEventsFailed();
  StHbtTrkV0MatchCollection* TrkV0MatchCollection();

  virtual StHbtString Report();
  virtual bool Pass(const StHbtEvent*);

  helensEventCut* Clone();

private:   // here are the quantities I want to cut on...

  int mEventMult[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex
  int mV0Mult[2];         // range of v0 multiplicity

  long mNEventsPassed;
  long mNEventsFailed;

  StHbtTrkV0MatchCollection* mTrkV0MatchCollection; //!

#ifdef __ROOT__
  ClassDef(helensEventCut, 1)
#endif

};

inline void helensEventCut::SetEventMult(const int& lo, const int& hi){mEventMult[0]=lo; mEventMult[1]=hi;}
inline void helensEventCut::SetV0Mult(const int& lo, const int& hi){mV0Mult[0]=lo; mV0Mult[1]=hi;}
inline void helensEventCut::SetVertZPos(const float& lo, const float& hi){mVertZPos[0]=lo; mVertZPos[1]=hi;}
inline int  helensEventCut::NEventsPassed() {return mNEventsPassed;}
inline int  helensEventCut::NEventsFailed() {return mNEventsFailed;}
inline helensEventCut* helensEventCut::Clone() { helensEventCut* c = new helensEventCut(*this); return c;}
inline StHbtTrkV0MatchCollection* helensEventCut::TrkV0MatchCollection(){return mTrkV0MatchCollection;}
inline helensEventCut::helensEventCut(helensEventCut& c) : StHbtEventCut(c) {
  mEventMult[0] = c.mEventMult[0];
  mEventMult[1] = c.mEventMult[1];
  mVertZPos[0] = c.mVertZPos[0];
  mVertZPos[1] = c.mVertZPos[1];
  mV0Mult[0] = c.mV0Mult[0];
  mV0Mult[1] = c.mV0Mult[1];
  mNEventsPassed = 0;
  mNEventsFailed = 0;
}


#endif
