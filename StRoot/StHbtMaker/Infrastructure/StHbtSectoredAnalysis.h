/***************************************************************************
 *
 * $Id: StHbtSectoredAnalysis.h,v 1.1 2000/04/12 01:46:34 willson Exp $
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
 * Revision 1.1  2000/04/12 01:46:34  willson
 * Initial Installation
 *
 * 
 ***************************************************************************/

#ifndef StHbtSectoredAnalysis_hh
#define StHbtSectoredAnalysis_hh
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif


#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
//#include <string>
#include "StHbtMaker/Base/StHbtEventCut.h"             // base class
#include "StHbtMaker/Base/StHbtParticleCut.h"          // base class
#include "StHbtMaker/Base/StHbtPairCut.h"              // base class
#include "StHbtMaker/Base/StHbtCorrFctn.hh"             // base class
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
  StHbtPairCut*          PairCut();
  void SetPairCut(StHbtPairCut*);
  
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

  bool SectoredMixingBufferFull();
  StHbtSectoredPicoEventCollection* SectoredMixingBuffer();

  StHbtCorrFctnCollection* CorrFctnCollection();
  StHbtCorrFctn* CorrFctn(int n);    // Access to CFs within the collection

  void AddCorrFctn(StHbtCorrFctn*);

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE

  virtual void Finish();


private:
  StHbtPairCut*          mPairCut;
  float                  mPXmax;
  float                  mPXmin;
  float                  mPYmax;
  float                  mPYmin;
  float                  mPZmax;
  float                  mPZmin;
  float                  mDeltaP;
  int                    mNumBinsX;
  int                    mNumBinsY;
  int                    mNumBinsZ;
  StHbtSectoredPicoEventCollection*  mSectoredMixingBuffer;
  StHbtCorrFctnCollection* mCorrFctnCollection;

#ifdef __ROOT__
  ClassDef(StHbtSectoredAnalysis, 0)
#endif

};

inline bool StHbtSectoredAnalysis::SectoredMixingBufferFull(){return (mSectoredMixingBuffer->size() >= NumEventsToMix());}
inline StHbtSectoredPicoEventCollection*  StHbtSectoredAnalysis::SectoredMixingBuffer() {return mSectoredMixingBuffer;}

// Get's
inline StHbtPairCut*          StHbtSectoredAnalysis::PairCut() {return mPairCut;}
inline float                  StHbtSectoredAnalysis::PXmax() {return mPXmax;}
inline float                  StHbtSectoredAnalysis::PXmin() {return mPXmin;}
inline float                  StHbtSectoredAnalysis::PYmax() {return mPYmax;}
inline float                  StHbtSectoredAnalysis::PYmin() {return mPYmin;}
inline float                  StHbtSectoredAnalysis::PZmax() {return mPZmax;}
inline float                  StHbtSectoredAnalysis::PZmin() {return mPZmin;}
inline float                  StHbtSectoredAnalysis::DeltaP() {return mDeltaP;}
inline int                    StHbtSectoredAnalysis::NumBinsX() {return mNumBinsX;}
inline int                    StHbtSectoredAnalysis::NumBinsY() {return mNumBinsY;}
inline int                    StHbtSectoredAnalysis::NumBinsZ() {return mNumBinsZ;}

inline StHbtCorrFctnCollection* StHbtSectoredAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}

// Set's
inline void StHbtSectoredAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); /*cf->myAnalysis=this;*/}
inline void StHbtSectoredAnalysis::SetPairCut(StHbtPairCut* x) { mPairCut = x; /*x->myAnalysis=this;*/}
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

inline bool StHbtSectoredAnalysis::AnalyzeIdenticalParticles() {return (mFirstParticleCut==mSecondParticleCut);}

#endif
