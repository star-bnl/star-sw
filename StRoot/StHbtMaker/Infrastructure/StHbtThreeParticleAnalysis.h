/***************************************************************************
 *
 * $Id: StHbtThreeParticleAnalysis.h,v 1.3 2000/05/11 21:18:56 willson Exp $
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
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif


#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class

class StHbtThreeParticleAnalysis : public StHbtBaseAnalysis {

public:

  StHbtThreeParticleAnalysis();
  virtual ~StHbtThreeParticleAnalysis();

  // Gets and Sets

  StHbtCorrFctn* CorrFctn(int n);     // Access to CFs within the collection

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE

  virtual void Finish();


private:

#ifdef __ROOT__
  ClassDef(StHbtThreeParticleAnalysis, 0)
#endif

};

// Set's

inline bool StHbtThreeParticleAnalysis::AnalyzeIdenticalParticles(){return ((mFirstParticleCut==mSecondParticleCut) && (mSecondParticleCut==mThirdParticleCut));}

#endif
