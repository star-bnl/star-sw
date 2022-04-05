/***************************************************************************
 *
 * $Id: StHbtSplitEvalAnalysis.h,v 1.2 2001/11/05 14:11:20 lisa Exp $
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
 * $Log: StHbtSplitEvalAnalysis.h,v $
 * Revision 1.2  2001/11/05 14:11:20  lisa
 * small modifications to Splitting Analysis class and macro
 *
 * Revision 1.1  2000/08/15 22:18:47  lisa
 * Add a special HbtAnalysis class that estimates amount of splitting and add a macro to use it
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
 * Copy constructor added to StHbtSplitEvalAnalysis (only known cuts, corrfctn).
 *
 * StHbtBinaryReader can now derive filename from StIOMaker and read a list
 * of files.
 *
 * StHbtManager now holds a collection of StHbtEventWriters (multiple writes
 * possible now)
 *
 * Revision 1.7  2000/02/13 17:17:12  laue
 * Calls to the EventBegin() and EventEnd() functions implemented
 * The actual analysis is moved from StHbtManager to StHbtSplitEvalAnalysis
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
 * include Franks new accessor methods StHbtSplitEvalAnalysis::CorrFctn and StHbtManager::Analysis as well as McEvent example macro
 *
 * Revision 1.2  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtSplitEvalAnalysis_hh
#define StHbtSplitEvalAnalysis_hh
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

//#include <string>

class StHbtSplitEvalAnalysis : public StHbtBaseAnalysis {

public:

  StHbtSplitEvalAnalysis();
  StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis&);  // copy constructor
  virtual ~StHbtSplitEvalAnalysis();

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

  unsigned int NumEventsToMix();
  void SetNumEventsToMix(const unsigned int&);
  StHbtPicoEventCollection* MixingBuffer();
  bool MixingBufferFull();

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE

  virtual void Finish();

  friend class StHbtLikeSignAnalysis;

  // just make these public for convenience...
  StHbt1DHisto*  mRealSplits;
  StHbt1DHisto*  mRealAll;
  StHbt1DHisto*  mMixedSplits;
  StHbt1DHisto*  mMixedAll;
  StHbt1DHisto*  mSplitFractionUpperLimit;
  StHbt1DHisto*  mSplitFractionLowerLimit;


  void SetQinvCut(float qc);

private:
  
  StHbtPairCut*             mPairCut;
  StHbtCorrFctnCollection*  mCorrFctnCollection;
  StHbtEventCut*            mEventCut;
  StHbtParticleCut*         mFirstParticleCut;
  StHbtParticleCut*         mSecondParticleCut;
  StHbtPicoEventCollection*  mMixingBuffer;
  unsigned int mNumEventsToMix;

  float mQinvCut;    // look at all pairs within this Qinv range.  (GeV/c)

#ifdef __ROOT__
  ClassDef(StHbtSplitEvalAnalysis, 0)
#endif

};

inline void                      StHbtSplitEvalAnalysis::SetQinvCut(float qc){mQinvCut=qc;}

// Get's
inline StHbtPairCut*             StHbtSplitEvalAnalysis::PairCut() {return mPairCut;}
inline StHbtEventCut*            StHbtSplitEvalAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*         StHbtSplitEvalAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*         StHbtSplitEvalAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline StHbtCorrFctnCollection*  StHbtSplitEvalAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}
inline unsigned int              StHbtSplitEvalAnalysis::NumEventsToMix(){return mNumEventsToMix;}

inline StHbtPicoEventCollection*  StHbtSplitEvalAnalysis::MixingBuffer() {return mMixingBuffer;}

// Set's
inline bool StHbtSplitEvalAnalysis::AnalyzeIdenticalParticles(){return (mFirstParticleCut==mSecondParticleCut);}
inline void StHbtSplitEvalAnalysis::SetPairCut(StHbtPairCut* x) { mPairCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtSplitEvalAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtSplitEvalAnalysis::SetEventCut(StHbtEventCut* x) {mEventCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtSplitEvalAnalysis::SetFirstParticleCut(StHbtParticleCut* x) {mFirstParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtSplitEvalAnalysis::SetSecondParticleCut(StHbtParticleCut* x) {mSecondParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}

inline void StHbtSplitEvalAnalysis::SetNumEventsToMix(const unsigned int& nmix){ mNumEventsToMix = nmix;}
inline bool StHbtSplitEvalAnalysis::MixingBufferFull(){return (mMixingBuffer->size() >= mNumEventsToMix);}


#endif
