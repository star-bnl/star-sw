/***************************************************************************
 *
 * $Id: StHbtVertexAnalysis.h,v 1.3 2000/08/11 16:35:41 rcwells Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtVertexAnalysis.h,v $
 * Revision 1.3  2000/08/11 16:35:41  rcwells
 * Added number of events processed to each HBT analysis
 *
 * Revision 1.2  2000/07/16 22:23:17  laue
 * I forgot that we moved memberfunctions out of StHbtBaseAnalysis.
 * So my previous check-ins didn't compile with the library.
 * Now they do.
 *
 * Revision 1.1  2000/07/16 21:44:11  laue
 * Collection and analysis for vertex dependent event mixing
 *
 *
 **************************************************************************/

#ifndef StHbtVertexAnalysis_hh
#define StHbtVertexAnalysis_hh

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"        // need this to get the FillHbtParticleCollection function
#include "StHbtMaker/Infrastructure/StHbtPicoEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"
//#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVector.hh"
//#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVectorHideAway.hh"
class StHbtPicoEventCollectionVectorHideAway;

class StHbtVertexAnalysis : public StHbtBaseAnalysis {

public:

  StHbtVertexAnalysis(unsigned int =10, double =-100., double=+100.);
  StHbtVertexAnalysis(const StHbtVertexAnalysis&);  // copy constructor
  virtual ~StHbtVertexAnalysis();

  // Gets and Sets

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

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

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE
  int GetNeventsProcessed();


  virtual void Finish();

  friend class StHbtLikeSignAnalysis;

private:

  void AddEventProcessed();

  double mVertexZ[2];
  unsigned int mVertexBins;
  unsigned int mNumEventsToMix;
  unsigned int mNeventsProcessed;
  StHbtPairCut*             mPairCut;
  StHbtCorrFctnCollection*  mCorrFctnCollection;
  StHbtEventCut*            mEventCut;
  StHbtParticleCut*         mFirstParticleCut;
  StHbtParticleCut*         mSecondParticleCut;
  StHbtPicoEventCollection*  mMixingBuffer;
  StHbtPicoEventCollectionVectorHideAway* mPicoEventCollectionVectorHideAway;
  
#ifdef __ROOT__
  ClassDef(StHbtVertexAnalysis, 0)
#endif
    
};

inline StHbtPairCut*             StHbtVertexAnalysis::PairCut() {return mPairCut;}
inline StHbtEventCut*            StHbtVertexAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*         StHbtVertexAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*         StHbtVertexAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline StHbtCorrFctnCollection*  StHbtVertexAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}
inline unsigned int              StHbtVertexAnalysis::NumEventsToMix(){return mNumEventsToMix;}
inline StHbtPicoEventCollection*  StHbtVertexAnalysis::MixingBuffer() {return mMixingBuffer;}

// Set's
inline bool StHbtVertexAnalysis::AnalyzeIdenticalParticles(){return (mFirstParticleCut==mSecondParticleCut);}
inline void StHbtVertexAnalysis::SetPairCut(StHbtPairCut* x) { mPairCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtVertexAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtVertexAnalysis::SetEventCut(StHbtEventCut* x) {mEventCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtVertexAnalysis::SetFirstParticleCut(StHbtParticleCut* x) {mFirstParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtVertexAnalysis::SetSecondParticleCut(StHbtParticleCut* x) {mSecondParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}

inline void StHbtVertexAnalysis::SetNumEventsToMix(const unsigned int& nmix){ mNumEventsToMix = nmix;}
inline bool StHbtVertexAnalysis::MixingBufferFull(){return (mMixingBuffer->size() >= mNumEventsToMix);}
inline int StHbtVertexAnalysis::GetNeventsProcessed() {return mNeventsProcessed;}


#endif
