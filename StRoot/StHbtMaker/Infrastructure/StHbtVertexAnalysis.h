/***************************************************************************
 *
 * $Id: StHbtVertexAnalysis.h,v 1.1 2000/07/16 21:44:11 laue Exp $
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

  StHbtCorrFctn* CorrFctn(int n);     // Access to CFs within the collection

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE
  

  virtual void Finish();

  friend class StHbtLikeSignAnalysis;

private:
  double mVertexZ[2];
  unsigned int mVertexBins;
  StHbtPicoEventCollectionVectorHideAway* mPicoEventCollectionVectorHideAway;
  
#ifdef __ROOT__
  ClassDef(StHbtVertexAnalysis, 0)
#endif
    
};

// Set's
inline bool StHbtVertexAnalysis::AnalyzeIdenticalParticles(){return (mFirstParticleCut==mSecondParticleCut);} //!

#endif
