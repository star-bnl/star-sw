/***************************************************************************
 *
 * $Id: StHbtBaseAnalysis.h,v 1.3 2000/04/12 01:53:00 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 * 
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   base class for an HBT analysis.  Users should use one of the
 *   inherited analysis classes with this class.
 *
 ***************************************************************************
 *
 * $Log: StHbtBaseAnalysis.h,v $
 * Revision 1.3  2000/04/12 01:53:00  willson
 * Initial Installation - Comments Added
 *
 *
 ***************************************************************************/

#ifndef StHbtBaseAnalysis_hh
#define StHbtBaseAnalysis_hh
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtParticleCut.h"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"

class StHbtMixingBuffer;

class StHbtBaseAnalysis{

public:

  StHbtBaseAnalysis() { /* noop */ };
   virtual ~StHbtBaseAnalysis() { /* noop */ };

#ifdef __ROOT__
  ClassDef(StHbtBaseAnalysis, 0)
#endif

  // Gets and Sets
  StHbtEventCut*      EventCut();
  StHbtParticleCut*   FirstParticleCut();
  StHbtParticleCut*   SecondParticleCut();
  
  void SetEventCut(StHbtEventCut*);
  void SetFirstParticleCut(StHbtParticleCut*);
  void SetSecondParticleCut(StHbtParticleCut*);
 
  unsigned int NumEventsToMix();
  void SetNumEventsToMix(const unsigned int&);
  StHbtPicoEventCollection* MixingBuffer();
  bool MixingBufferFull();
 
  virtual StHbtString Report() = 0;       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*) = 0;
  // virtual void EventBegin(const StHbtEvent*);
  // virtual void EventEnd(const StHbtEvent*);

  virtual void Finish() = 0;

protected:
  StHbtEventCut*      mEventCut;
  StHbtParticleCut*   mFirstParticleCut;
  StHbtParticleCut*   mSecondParticleCut;

  StHbtPicoEventCollection*  mMixingBuffer;
  unsigned int mNumEventsToMix;

};

// Get's
inline StHbtEventCut*      StHbtBaseAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*     StHbtBaseAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*     StHbtBaseAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline unsigned int        StHbtBaseAnalysis::NumEventsToMix(){return mNumEventsToMix;}

inline StHbtPicoEventCollection*  StHbtBaseAnalysis::MixingBuffer() {return mMixingBuffer;}

// Set's

inline void StHbtBaseAnalysis::SetEventCut(StHbtEventCut* x) {mEventCut = x; x->myAnalysis = (StHbtBaseAnalysis*) this;}
inline void StHbtBaseAnalysis::SetFirstParticleCut(StHbtParticleCut* x) {mFirstParticleCut = x; x->myAnalysis = (StHbtBaseAnalysis*) this;}
inline void StHbtBaseAnalysis::SetSecondParticleCut(StHbtParticleCut* x) {mSecondParticleCut = x; x->myAnalysis = (StHbtBaseAnalysis*) this;}

inline void StHbtBaseAnalysis::SetNumEventsToMix(const unsigned int& nmix){ mNumEventsToMix = nmix;}

inline bool StHbtBaseAnalysis::MixingBufferFull(){return (mMixingBuffer->size() >= mNumEventsToMix);}



#endif

