#ifndef StParityAnalysis_hh
#define StParityAnalysis_hh
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Base/StHbtPairCut.h"     
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtParticleCut.h"
#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/CorrFctn/ParityDevCorrFctn.h"
#include "StHbtMaker/Infrastructure/StParityTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtCorrFctnCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"

#define SAME 5 
#define MIXED 6 

class StParityAnalysis : public StHbtBaseAnalysis {
  //  int   EvanTest;
public:

  StParityAnalysis();
  StParityAnalysis(const StParityAnalysis&);  // copy constructor
  virtual ~StParityAnalysis();

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
  int GetNeventsProcessed();

  virtual void Finish();

  friend class StHbtLikeSignAnalysis;


  private:

  void AddEventProcessed();

  StHbtPairCut*             mPairCut;
  StHbtCorrFctnCollection*  mCorrFctnCollection;
  StHbtEventCut*            mEventCut;
  StHbtParticleCut*         mFirstParticleCut;
  StHbtParticleCut*         mSecondParticleCut;
  StHbtPicoEventCollection*  mMixingBuffer;
  unsigned int mNumEventsToMix;
  unsigned int mNeventsProcessed;



#ifdef __ROOT__
  ClassDef(StParityAnalysis, 0)
#endif

};
// Get's

inline StHbtPairCut*             StParityAnalysis::PairCut() {return mPairCut;}
inline StHbtEventCut*            StParityAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*         StParityAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*         StParityAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline StHbtCorrFctnCollection*  StParityAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}
inline unsigned int              StParityAnalysis::NumEventsToMix(){return mNumEventsToMix;}

inline StHbtPicoEventCollection*  StParityAnalysis::MixingBuffer() {return mMixingBuffer;}

// Set's
inline bool StParityAnalysis::AnalyzeIdenticalParticles(){return (mFirstParticleCut==mSecondParticleCut);}
inline void StParityAnalysis::SetPairCut(StHbtPairCut* x) { mPairCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StParityAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StParityAnalysis::SetEventCut(StHbtEventCut* x) {mEventCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StParityAnalysis::SetFirstParticleCut(StHbtParticleCut* x) {mFirstParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StParityAnalysis::SetSecondParticleCut(StHbtParticleCut* x) {mSecondParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}

inline void StParityAnalysis::SetNumEventsToMix(const unsigned int& nmix){ mNumEventsToMix = nmix;}
inline bool StParityAnalysis::MixingBufferFull(){return (mMixingBuffer->size() >= mNumEventsToMix);}
inline int StParityAnalysis::GetNeventsProcessed() {return mNeventsProcessed;}


#endif
