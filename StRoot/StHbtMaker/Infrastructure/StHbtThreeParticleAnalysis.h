/***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the derived Class for three particle Analysis objects.  
 *      Each of the simultaneous Analyses should have one of derived 
 *      Analysis classes runningthese instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************/


#ifndef StHbtThreeParticleAnalysis_hh
#define StHbtThreeParticleAnalysis_hh
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif


#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Base/StHbtTripletCut.h"              // base class
#include "StHbtMaker/Base/StHbtThreeParticleCorrFctn.hh"             // base class
#include "StHbtMaker/Infrastructure/StHbtThreeParticleCorrFctnCollection.hh"

class StHbtThreeParticleAnalysis : public StHbtBaseAnalysis {

public:

  StHbtThreeParticleAnalysis();
  virtual ~StHbtThreeParticleAnalysis();

  // Gets and Sets
  StHbtTripletCut*       TripletCut();
  StHbtParticleCut*      ThirdParticleCut();

  void SetThirdParticleCut(StHbtParticleCut*);
  void SetTripletCut(StHbtTripletCut*);

  StHbtThreeParticleCorrFctnCollection* CorrFctnCollection();
  StHbtThreeParticleCorrFctn* CorrFctn(int n);    // Access to CFs within the collection

  void AddCorrFctn(StHbtThreeParticleCorrFctn*);

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE

  virtual void Finish();


private:
  StHbtTripletCut*       mTripletCut;
  StHbtParticleCut*      mThirdParticleCut;

  StHbtThreeParticleCorrFctnCollection* mCorrFctnCollection;

#ifdef __ROOT__
  ClassDef(StHbtThreeParticleAnalysis, 0)
#endif

};

// Get's
inline StHbtTripletCut*       StHbtThreeParticleAnalysis::TripletCut() {return mTripletCut;}
inline StHbtParticleCut*      StHbtThreeParticleAnalysis::ThirdParticleCut() {return mThirdParticleCut;}
inline StHbtThreeParticleCorrFctnCollection* StHbtThreeParticleAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}

// Set's
inline void StHbtThreeParticleAnalysis::SetThirdParticleCut(StHbtParticleCut* x) {mThirdParticleCut = x; /*x->myAnalysis = (StHbtBaseAnalysis) this;*/}
inline void StHbtThreeParticleAnalysis::AddCorrFctn(StHbtThreeParticleCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->myAnalysis=this;}
inline void StHbtThreeParticleAnalysis::SetTripletCut(StHbtTripletCut* x) { mTripletCut = x; x->myAnalysis=this;}

inline bool StHbtThreeParticleAnalysis::AnalyzeIdenticalParticles(){return ((mFirstParticleCut==mSecondParticleCut) && (mSecondParticleCut==mThirdParticleCut));}

#endif
