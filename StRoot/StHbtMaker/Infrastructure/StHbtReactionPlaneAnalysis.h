/***************************************************************************
 *
 * $Id: StHbtReactionPlaneAnalysis.h,v 1.1 2001/07/13 20:03:14 rcwells Exp $
 *
 * Author: Randall Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtReactionPlaneAnalysis.h,v $
 * Revision 1.1  2001/07/13 20:03:14  rcwells
 * Adding reaction plane analysis
 *
 *
 **************************************************************************/

#ifndef StHbtReactionPlaneAnalysis_hh
#define StHbtReactionPlaneAnalysis_hh

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"        // base analysis class
class StHbtPicoEventCollectionVectorHideAway;

class StHbtReactionPlaneAnalysis : public StHbtAnalysis {

public:

  StHbtReactionPlaneAnalysis(int=1, unsigned int =10, double =-100., double=+100.);
  StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis&);  // copy constructor
  virtual void ProcessEvent(const StHbtEvent*);
  virtual ~StHbtReactionPlaneAnalysis();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done
  virtual unsigned int Overflow() { return mOverFlow;}
  virtual unsigned int Underflow() { return mUnderFlow;}
  double ReactionPlane();
  int PtWeighting();
protected:
  int mPtWgt;
  double mReactionPlaneAngle;
  double mReactionPlane[2];
  unsigned int mReactionPlaneBins;
  unsigned int mOverFlow;
  unsigned int mUnderFlow;
  StHbtPicoEventCollectionVectorHideAway* mPicoEventCollectionVectorHideAway;
  
#ifdef __ROOT__
  ClassDef(StHbtReactionPlaneAnalysis, 0)
#endif
    
};

#endif
