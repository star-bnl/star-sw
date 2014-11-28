/***************************************************************************
 *
 * $Id: StHbtLikeSignAnalysis.h,v 1.3 2002/11/03 16:37:43 magestro Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *      This is an analysis which calculated the background from like sign
 *      pairs in the same event
 *
 ***************************************************************************/


#ifndef StHbtLikeSignAnalysis_hh
#define StHbtLikeSignAnalysis_hh
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"             // base class 
#include "StHbtMaker/Base/StHbtParticleCut.h"          // base class
#include "StHbtMaker/Base/StHbtPairCut.h"              // base class
#include "StHbtMaker/Base/StHbtLikeSignCorrFctn.hh"    // base class
#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtCorrFctnCollection.hh"


class StHbtLikeSignAnalysis : public StHbtAnalysis {

public: 

  StHbtLikeSignAnalysis(unsigned int bins=20, double min=-100., double max=100.);
  StHbtLikeSignAnalysis(const StHbtLikeSignAnalysis&);  // copy constructor
  virtual ~StHbtLikeSignAnalysis();

  virtual void ProcessEvent(const StHbtEvent*);
  virtual StHbtString Report();
  virtual unsigned int Overflow() { return mOverFlow;}
  virtual unsigned int Underflow() { return mUnderFlow;}

protected:
  double mVertexZ[2];
  unsigned int mVertexBins;
  unsigned int mOverFlow;
  unsigned int mUnderFlow;

#ifdef __ROOT__
  ClassDef(StHbtLikeSignAnalysis, 0)
#endif

};


#endif
