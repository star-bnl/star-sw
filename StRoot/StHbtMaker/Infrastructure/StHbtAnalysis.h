/***************************************************************************
 *
 * $Id: StHbtAnalysis.h,v 1.17 2002/11/03 16:38:54 magestro Exp $
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
 * Revision 1.17  2002/11/03 16:38:54  magestro
 * New MakePairs() method, new StHbtPicoEventCollectionVectorHideAway data member
 *
 * Revision 1.16  2002/06/22 17:53:32  lisa
 * implemented switch to allow user to require minimum number of particles in First and Second ParticleCollections - default value is zero so if user does not Set this value then behaviour is like before
 *
 * Revision 1.15  2001/04/05 21:57:45  laue
 * current pico-event becomes a member of the analysis (mPicoEvent) and gets
 * an access-function (CurrentPicoEvent)
 *
 * Revision 1.14  2000/08/31 22:31:30  laue
 * StHbtAnalysis: output changed (a little bit less)
 * StHbtEvent: new version, members for reference mult added
 * StHbtIOBinary: new IO for new StHbtEvent version
 * StHbtTypes: TTree typedef to StHbtTTree added
 * StHbtVertexAnalysis: overflow and underflow added
 *
 * Revision 1.13  2000/08/11 16:35:41  rcwells
 * Added number of events processed to each HBT analysis
 *
 * Revision 1.12  2000/06/15 18:54:08  willson
 * Methods to access cuts and correlation functions moved to derived analysis
 * classes.
 *
 * Revision 1.11  2000/05/11 21:18:56  willson
 * Removed StHbtThreeParticleCorrFctn's...put methods in StHbtCorrFctn
 * Some methods in derived analysis classes moved to base analysis class
 *
 * Revision 1.10  2000/04/03 16:21:50  laue
 * some include files changed
 * Multi track cut added
 *
 * Revision 1.9  2000/03/17 17:23:05  laue
 * Roberts new three particle correlations implemented.
 *
 * Revision 1.8  2000/03/16 02:07:04  laue
 * Copy constructor added to StHbtAnalysis (only known cuts, corrfctn).
 *
 * StHbtBinaryReader can now derive filename from StIOMaker and read a list
 * of files.
 *
 * StHbtManager now holds a collection of StHbtEventWriters (multiple writes
 * possible now)
 *
 * Revision 1.7  2000/02/13 17:17:12  laue
 * Calls to the EventBegin() and EventEnd() functions implemented
 * The actual analysis is moved from StHbtManager to StHbtAnalysis
 *
 * Revision 1.6  2000/01/25 17:35:17  laue
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
 * Revision 1.5  1999/12/03 22:24:36  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
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
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Base/StHbtPairCut.h"     
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtParticleCut.h"
#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtCorrFctnCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEvent.hh"

class StHbtPicoEventCollectionVectorHideAway;


class StHbtAnalysis : public StHbtBaseAnalysis {

public:

  StHbtAnalysis();
  StHbtAnalysis(const StHbtAnalysis&);  // copy constructor
  virtual ~StHbtAnalysis();

  // Gets and Sets

  virtual StHbtPairCut*       PairCut();
  virtual StHbtEventCut*      EventCut();
  virtual StHbtParticleCut*   FirstParticleCut();
  virtual StHbtParticleCut*   SecondParticleCut();

  StHbtCorrFctnCollection* CorrFctnCollection();
  virtual StHbtCorrFctn* CorrFctn(int n);     // Access to CFs within the collection
  void AddCorrFctn(StHbtCorrFctn*);

  void SetPairCut(StHbtPairCut*);
  void SetEventCut(StHbtEventCut*);
  void SetFirstParticleCut(StHbtParticleCut*);
  void SetSecondParticleCut(StHbtParticleCut*);

  void SetMinSizePartCollection(unsigned int minSize);


  unsigned int NumEventsToMix();
  void SetNumEventsToMix(const unsigned int&);
  StHbtPicoEvent* CurrentPicoEvent();
  StHbtPicoEventCollection* MixingBuffer();
  bool MixingBufferFull();

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  virtual void EventBegin(const StHbtEvent*); // startup for EbyE
  virtual void EventEnd(const StHbtEvent*);   // cleanup for EbyE
  int GetNeventsProcessed();

  virtual void Finish();

  friend class StHbtLikeSignAnalysis;

protected:

  void AddEventProcessed();
  void MakePairs(const char* type,StHbtParticleCollection*,StHbtParticleCollection* p2=0);

  StHbtPicoEventCollectionVectorHideAway* mPicoEventCollectionVectorHideAway;

  StHbtPairCut*             mPairCut;
  StHbtCorrFctnCollection*  mCorrFctnCollection;
  StHbtEventCut*            mEventCut;
  StHbtParticleCut*         mFirstParticleCut;
  StHbtParticleCut*         mSecondParticleCut;
  StHbtPicoEventCollection* mMixingBuffer;
  StHbtPicoEvent*           mPicoEvent;
  unsigned int mNumEventsToMix;
  unsigned int mNeventsProcessed;

  unsigned int mMinSizePartCollection;  // minimum # particles in ParticleCollection


#ifdef __ROOT__
  ClassDef(StHbtAnalysis, 0)
#endif

};

// Get's
inline StHbtPairCut*             StHbtAnalysis::PairCut() {return mPairCut;}
inline StHbtEventCut*            StHbtAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*         StHbtAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*         StHbtAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline StHbtCorrFctnCollection*  StHbtAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}
inline unsigned int              StHbtAnalysis::NumEventsToMix(){return mNumEventsToMix;}
inline StHbtPicoEvent*           StHbtAnalysis::CurrentPicoEvent() {return mPicoEvent;}

inline StHbtPicoEventCollection*  StHbtAnalysis::MixingBuffer() {return mMixingBuffer;}

// Set's
inline bool StHbtAnalysis::AnalyzeIdenticalParticles(){return (mFirstParticleCut==mSecondParticleCut);}
inline void StHbtAnalysis::SetPairCut(StHbtPairCut* x) { mPairCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtAnalysis::SetEventCut(StHbtEventCut* x) {mEventCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtAnalysis::SetFirstParticleCut(StHbtParticleCut* x) {mFirstParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtAnalysis::SetSecondParticleCut(StHbtParticleCut* x) {mSecondParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}

inline void StHbtAnalysis::SetNumEventsToMix(const unsigned int& nmix){ mNumEventsToMix = nmix;}
inline bool StHbtAnalysis::MixingBufferFull(){return (mMixingBuffer->size() >= mNumEventsToMix);}
inline int StHbtAnalysis::GetNeventsProcessed() {return mNeventsProcessed;}

inline void StHbtAnalysis::SetMinSizePartCollection(unsigned int minSize){mMinSizePartCollection = minSize;}

#endif
