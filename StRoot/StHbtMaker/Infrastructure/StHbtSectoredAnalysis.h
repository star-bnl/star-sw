/***************************************************************************
 *
 * $Id: StHbtSectoredAnalysis.h,v 1.4 2000/08/11 16:35:41 rcwells Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *  
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *      This is a derived class for two-particle analysis objects.  
 *      In this analysis, momentum space is sectored into a number of
 *      cubes, which are then mixed with nearest neighbors.  Several
 *      initializations are needed to set up the sectoring, see 
 *      StHbtSectoredExample.C in the /doc directory for use.
 *
 ***************************************************************************
 *
 * $Log: StHbtSectoredAnalysis.h,v $
 * Revision 1.4  2000/08/11 16:35:41  rcwells
 * Added number of events processed to each HBT analysis
 *
 * Revision 1.3  2000/06/15 18:54:08  willson
 * Methods to access cuts and correlation functions moved to derived analysis
 * classes.
 *
 * Revision 1.2  2000/05/11 21:18:56  willson
 * Removed StHbtThreeParticleCorrFctn's...put methods in StHbtCorrFctn
 * Some methods in derived analysis classes moved to base analysis class
 *
 * Revision 1.1  2000/04/12 01:46:34  willson
 * Initial Installation
 *
 * 
 ***************************************************************************/

#ifndef StHbtSectoredAnalysis_hh
#define StHbtSectoredAnalysis_hh

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Base/StHbtPairCut.h"   
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtParticleCut.h"
#include "StHbtMaker/Base/StHbtCorrFctn.hh" 
#include "StHbtMaker/Infrastructure/StHbtCorrFctnCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtSectoredPicoEventCollection.hh"

class StHbtSectoredAnalysis : public StHbtBaseAnalysis {

public:

  StHbtSectoredAnalysis(); 
  StHbtSectoredAnalysis(const StHbtSectoredAnalysis&);  // copy constructor
  virtual ~StHbtSectoredAnalysis();

  void CreateRealPairs(StHbtParticleCollection*);
  void CreateRealPairs(StHbtParticleCollection*, StHbtParticleCollection*);
  void CreateMixedPairs(StHbtParticleCollection*, StHbtParticleCollection*);
  void SortHbtParticleCollection(StHbtParticleCut*, StHbtEvent*, StHbtParticleCollection**);
  int  Index(int, int, int);
 
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



  int                    NumBinsX();
  int                    NumBinsY();
  int                    NumBinsZ();
  float                  PXmax();
  float                  PXmin();
  float                  PYmax();
  float                  PYmin();
  float                  PZmax();
  float                  PZmin();
  float                  DeltaP();

  void SetPXmax(float);
  void SetPXmin(float);
  void SetPYmax(float);
  void SetPYmin(float);
  void SetPZmax(float);
  void SetPZmin(float);
  void SetDeltaP(float);

  unsigned int NumEventsToMix();
  void SetNumEventsToMix(const unsigned int&);
  StHbtSectoredPicoEventCollection* SectoredMixingBuffer();
  bool SectoredMixingBufferFull();

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE
  int GetNeventsProcessed();

  virtual void Finish();


private:

  void AddEventProcessed();

  StHbtPairCut*                      mPairCut;
  StHbtCorrFctnCollection*           mCorrFctnCollection;
  StHbtEventCut*                     mEventCut;
  StHbtParticleCut*                  mFirstParticleCut;
  StHbtParticleCut*                  mSecondParticleCut;
  float                              mPXmax;
  float                              mPXmin;
  float                              mPYmax;
  float                              mPYmin;
  float                              mPZmax;
  float                              mPZmin;
  float                              mDeltaP;
  int                                mNumBinsX;
  int                                mNumBinsY;
  int                                mNumBinsZ;
  StHbtSectoredPicoEventCollection*  mSectoredMixingBuffer;
  unsigned int                       mNumEventsToMix;
  unsigned int                       mNeventsProcessed;


#ifdef __ROOT__
  ClassDef(StHbtSectoredAnalysis, 0)
#endif

};

// Get's
inline StHbtPairCut*                      StHbtSectoredAnalysis::PairCut() {return mPairCut;}
inline StHbtEventCut*                     StHbtSectoredAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*                  StHbtSectoredAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*                  StHbtSectoredAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline StHbtCorrFctnCollection*           StHbtSectoredAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}
inline unsigned int                       StHbtSectoredAnalysis::NumEventsToMix(){return mNumEventsToMix;}
inline StHbtSectoredPicoEventCollection*  StHbtSectoredAnalysis::SectoredMixingBuffer() {return mSectoredMixingBuffer;}

inline float                     StHbtSectoredAnalysis::PXmax() {return mPXmax;}
inline float                     StHbtSectoredAnalysis::PXmin() {return mPXmin;}
inline float                     StHbtSectoredAnalysis::PYmax() {return mPYmax;}
inline float                     StHbtSectoredAnalysis::PYmin() {return mPYmin;}
inline float                     StHbtSectoredAnalysis::PZmax() {return mPZmax;}
inline float                     StHbtSectoredAnalysis::PZmin() {return mPZmin;}
inline float                     StHbtSectoredAnalysis::DeltaP() {return mDeltaP;}
inline int                       StHbtSectoredAnalysis::NumBinsX() {return mNumBinsX;}
inline int                       StHbtSectoredAnalysis::NumBinsY() {return mNumBinsY;}
inline int                       StHbtSectoredAnalysis::NumBinsZ() {return mNumBinsZ;}

// Set's

inline bool StHbtSectoredAnalysis::AnalyzeIdenticalParticles() {return (mFirstParticleCut==mSecondParticleCut);}
inline void StHbtSectoredAnalysis::SetPairCut(StHbtPairCut* x) { mPairCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtSectoredAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtSectoredAnalysis::SetEventCut(StHbtEventCut* x) {mEventCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtSectoredAnalysis::SetFirstParticleCut(StHbtParticleCut* x) {mFirstParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtSectoredAnalysis::SetSecondParticleCut(StHbtParticleCut* x) {mSecondParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}

inline void StHbtSectoredAnalysis::SetNumEventsToMix(const unsigned int& nmix){ mNumEventsToMix = nmix;}
inline bool StHbtSectoredAnalysis::SectoredMixingBufferFull(){return (mSectoredMixingBuffer->size() >= NumEventsToMix());}

inline void StHbtSectoredAnalysis::SetPXmax(float x) {
  mPXmax = x;
  if (mPXmax<mPXmin) mNumBinsX=1;
  else mNumBinsX = (int)ceil((mPXmax-mPXmin)/mDeltaP);
  if (mNumBinsX==0) mNumBinsX=1;
}
inline void StHbtSectoredAnalysis::SetPXmin(float x) {
  mPXmin = x;
  if (mPXmax<mPXmin) mNumBinsX=1;
  else mNumBinsX = (int)ceil((mPXmax-mPXmin)/mDeltaP);
  if (mNumBinsX==0) mNumBinsX=1;
}
inline void StHbtSectoredAnalysis::SetPYmax(float x) {
  mPYmax = x;
  if (mPYmax<mPYmin) mNumBinsY=1;
  else mNumBinsY = (int)ceil((mPYmax-mPYmin)/mDeltaP);
  if (mNumBinsY==0) mNumBinsY=1;
}
inline void StHbtSectoredAnalysis::SetPYmin(float x) {
  mPYmin = x;
  if (mPYmax<mPYmin) mNumBinsY=1;
  else mNumBinsY = (int)ceil((mPYmax-mPYmin)/mDeltaP);
  if (mNumBinsY==0) mNumBinsY=1;
}
inline void StHbtSectoredAnalysis::SetPZmax(float x) {
  mPZmax = x;
  if (mPZmax<mPZmin) mNumBinsZ=1;
  else mNumBinsZ = (int)ceil((mPZmax-mPZmin)/mDeltaP);
  if (mNumBinsZ==0) mNumBinsZ=1;
}
inline void StHbtSectoredAnalysis::SetPZmin(float x) {
  mPZmin = x;
  if (mPZmax<mPZmin) mNumBinsZ=1;
  else mNumBinsZ = (int)ceil((mPZmax-mPZmin)/mDeltaP);
  if (mNumBinsZ==0) mNumBinsZ=1;
}
inline void StHbtSectoredAnalysis::SetDeltaP(float x) {
  if (x<=0) {
    mDeltaP = 1.0;
    cout << "****ERROR****  DeltaP must be greater than zero...setting DeltaP to 1.0" << endl;
  }
  else {
    mDeltaP = x;
    mNumBinsX = (int)ceil((mPXmax-mPXmin)/mDeltaP);
    if (mNumBinsX==0) mNumBinsX=1;
    mNumBinsY = (int)ceil((mPYmax-mPYmin)/mDeltaP);
    if (mNumBinsY==0) mNumBinsY=1;
    mNumBinsZ = (int)ceil((mPZmax-mPZmin)/mDeltaP);
    if (mNumBinsZ==0) mNumBinsZ=1;
  }
}

#endif

inline int StHbtSectoredAnalysis::GetNeventsProcessed() {return mNeventsProcessed;}
