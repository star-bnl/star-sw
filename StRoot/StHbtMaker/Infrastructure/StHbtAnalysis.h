/***************************************************************************
 *
 * $Id: StHbtAnalysis.h,v 1.4 1999/10/15 01:57:23 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtAnalysis.h,v $
 * Revision 1.4  1999/10/15 01:57:23  lisa
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
 * Revision 1.3  1999/10/04 15:38:56  lisa
 * include Franks new accessor methods StHbtAnalysis::CorrFctn and StHbtManager::Analysis as well as McEvent example macro
 *
 * Revision 1.2  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtAnalysis_hh
#define StHbtAnalysis_hh
#ifndef StMaker_H
#include "StMaker.h"
#endif


#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
//#include <string>
//#include "base/StHbtControlSwitch.hh"        // base class
#include "StHbtMaker/Base/StHbtEventCut.h"             // base class 
#include "StHbtMaker/Base/StHbtParticleCut.h"          // base class
#include "StHbtMaker/Base/StHbtPairCut.h"              // base class
#include "StHbtMaker/Base/StHbtCorrFctn.hh"             // base class
#include "StHbtMaker/Infrastructure/StHbtCorrFctnCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"

class StHbtMixingBuffer;

class StHbtAnalysis{

public:

  StHbtAnalysis();
  ~StHbtAnalysis();

  // Gets and Sets
  //  StHbtControlSwitch* ControlSwitch();
  StHbtEventCut*      EventCut();
  StHbtParticleCut*   FirstParticleCut();
  StHbtParticleCut*   SecondParticleCut();
  StHbtPairCut*       PairCut();

  //void SetControlSwitch(StHbtControlSwitch*);
  void SetEventCut(StHbtEventCut*);
  void SetFirstParticleCut(StHbtParticleCut*);
  void SetSecondParticleCut(StHbtParticleCut*);
  void SetPairCut(StHbtPairCut*);

  unsigned int NumEventsToMix();
  void SetNumEventsToMix(const unsigned int&);
  StHbtPicoEventCollection* MixingBuffer();
  bool MixingBufferFull();

  StHbtCorrFctnCollection* CorrFctnCollection();
  StHbtCorrFctn* CorrFctn(int n);    // Access to CFs within the collection

  void AddCorrFctn(StHbtCorrFctn*);

  bool AnalyzeIdenticalParticles();
  StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  void Finish();


private:
  //StHbtControlSwitch* mControlSwitch;
  StHbtEventCut*      mEventCut;
  StHbtParticleCut*   mFirstParticleCut;
  StHbtParticleCut*   mSecondParticleCut;
  StHbtPairCut*       mPairCut;

  StHbtPicoEventCollection*  mMixingBuffer;

  StHbtCorrFctnCollection* mCorrFctnCollection;

  unsigned int mNumEventsToMix;

  ClassDef(StHbtAnalysis, 0)


};

// Get's
//inline StHbtControlSwitch* StHbtAnalysis::ControlSwitch() {return mControlSwitch;}
inline StHbtEventCut*      StHbtAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*   StHbtAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*   StHbtAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline StHbtPairCut*       StHbtAnalysis::PairCut() {return mPairCut;}
inline unsigned int        StHbtAnalysis::NumEventsToMix(){return mNumEventsToMix;}

inline StHbtCorrFctnCollection* StHbtAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}
inline StHbtPicoEventCollection*  StHbtAnalysis::MixingBuffer() {return mMixingBuffer;}

// Set's
//inline void StHbtAnalysis::SetControlSwitch(StHbtControlSwitch* x) { mControlSwitch = x;}
inline void StHbtAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf);}
inline void StHbtAnalysis::SetEventCut(StHbtEventCut* x) { mEventCut = x;}
inline void StHbtAnalysis::SetFirstParticleCut(StHbtParticleCut* x) { mFirstParticleCut = x;}
inline void StHbtAnalysis::SetSecondParticleCut(StHbtParticleCut* x) { mSecondParticleCut = x;}
inline void StHbtAnalysis::SetPairCut(StHbtPairCut* x) { mPairCut = x;}

inline void StHbtAnalysis::SetNumEventsToMix(const unsigned int& nmix){ mNumEventsToMix = nmix;}

inline bool StHbtAnalysis::MixingBufferFull(){return (mMixingBuffer->size() >= mNumEventsToMix);}

inline bool StHbtAnalysis::AnalyzeIdenticalParticles(){return (mFirstParticleCut==mSecondParticleCut);}

#endif
