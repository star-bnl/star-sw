/***************************************************************************
 *
 * $Id: StHbtThreeParticleAnalysis.h,v 1.4 2000/06/15 18:54:08 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *  
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *      This is the derived class for three particle analysis objects.  
 *      Each of the simultaneous analyses should have one of derived 
 *      analysis classes running these instantiated.  They link
 *      into the manager in an analysis collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtThreeParticleAnalysis.h,v $
 * Revision 1.4  2000/06/15 18:54:08  willson
 * Methods to access cuts and correlation functions moved to derived analysis
 * classes.
 *
 * Revision 1.3  2000/05/11 21:18:56  willson
 * Removed StHbtThreeParticleCorrFctn's...put methods in StHbtCorrFctn
 * Some methods in derived analysis classes moved to base analysis class
 *
 * Revision 1.2  2000/04/12 01:54:33  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/

#ifndef StHbtThreeParticleAnalysis_hh
#define StHbtThreeParticleAnalysis_hh

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Base/StHbtTripletCut.h"              // base class
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtParticleCut.h"
#include "StHbtMaker/Base/StHbtCorrFctn.hh" 
#include "StHbtMaker/Infrastructure/StHbtCorrFctnCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"

class StHbtThreeParticleAnalysis : public StHbtBaseAnalysis {

public:

  StHbtThreeParticleAnalysis();
  virtual ~StHbtThreeParticleAnalysis();

  // Gets and Sets

  virtual StHbtTripletCut*    TripletCut();
  virtual StHbtEventCut*      EventCut();
  virtual StHbtParticleCut*   FirstParticleCut();
  virtual StHbtParticleCut*   SecondParticleCut();
  virtual StHbtParticleCut*   ThirdParticleCut();

  StHbtCorrFctnCollection* CorrFctnCollection();
  virtual StHbtCorrFctn* CorrFctn(int n);     // Access to CFs within the collection
  void AddCorrFctn(StHbtCorrFctn*);

  void SetTripletCut(StHbtTripletCut*);
  void SetEventCut(StHbtEventCut*);
  void SetFirstParticleCut(StHbtParticleCut*);
  void SetSecondParticleCut(StHbtParticleCut*);
  void SetThirdParticleCut(StHbtParticleCut*);

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


private:
  StHbtTripletCut*          mTripletCut;
  StHbtCorrFctnCollection*  mCorrFctnCollection;
  StHbtEventCut*            mEventCut;
  StHbtParticleCut*         mFirstParticleCut;
  StHbtParticleCut*         mSecondParticleCut;
  StHbtParticleCut*         mThirdParticleCut;
  StHbtPicoEventCollection*  mMixingBuffer;
  unsigned int mNumEventsToMix;

#ifdef __ROOT__
  ClassDef(StHbtThreeParticleAnalysis, 0)
#endif

};

// Get's

inline StHbtTripletCut*          StHbtThreeParticleAnalysis::TripletCut() {return mTripletCut;}
inline StHbtEventCut*            StHbtThreeParticleAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*         StHbtThreeParticleAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*         StHbtThreeParticleAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline StHbtParticleCut*         StHbtThreeParticleAnalysis::ThirdParticleCut() {return mThirdParticleCut;}
inline StHbtCorrFctnCollection*  StHbtThreeParticleAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}
inline unsigned int              StHbtThreeParticleAnalysis::NumEventsToMix(){return mNumEventsToMix;}

inline StHbtPicoEventCollection*  StHbtThreeParticleAnalysis::MixingBuffer() {return mMixingBuffer;}

// Set's

inline bool StHbtThreeParticleAnalysis::AnalyzeIdenticalParticles(){return ((mFirstParticleCut==mSecondParticleCut) && (mSecondParticleCut==mThirdParticleCut));}
inline void StHbtThreeParticleAnalysis::SetTripletCut(StHbtTripletCut* x) { mTripletCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::SetEventCut(StHbtEventCut* x) {mEventCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::SetFirstParticleCut(StHbtParticleCut* x) {mFirstParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::SetSecondParticleCut(StHbtParticleCut* x) {mSecondParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::SetThirdParticleCut(StHbtParticleCut* x) {mThirdParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}

#endif
