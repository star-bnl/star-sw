/***************************************************************************
 *
 * $Id: StHbtReactionPlaneAnalysis.h,v 1.4 2004/04/12 14:05:46 magestro Exp $
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
 * Revision 1.4  2004/04/12 14:05:46  magestro
 * Added Vz dimension to event-mixing
 *
 * Revision 1.2  2002/05/28 14:04:07  rcwells
 * Added multiplicity binning to StHbtReactionPlaneAnalysis
 *
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

  StHbtReactionPlaneAnalysis(int pTSwitch=1, unsigned int nBinPhi=1, double phiLo=-100., double phiHi=+100.,
			     unsigned int nBinsMult=1, double multLo=0, double multHi=999999,
           unsigned int nBinsVert=1, double vertLo=-1000, double vertHi=1000);
  StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis&);  // copy constructor
  virtual void ProcessEvent(const StHbtEvent*);
  virtual ~StHbtReactionPlaneAnalysis();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done
  virtual unsigned int Overflow() { return mOverFlow;}
  virtual unsigned int Underflow() { return mUnderFlow;}
  double ReactionPlane();
  double VertexZ();
  int PtWeighting();
protected:
  int mPtWgt;
  double mVertexZ;
  double mReactionPlaneAngle;
  double mReactionPlane[2];
  unsigned int mReactionPlaneBins;
  unsigned int mOverFlow;
  unsigned int mUnderFlow;
  
#ifdef __ROOT__
  ClassDef(StHbtReactionPlaneAnalysis, 0)
#endif
    
};

inline double StHbtReactionPlaneAnalysis::ReactionPlane(){return mReactionPlaneAngle;}
inline double StHbtReactionPlaneAnalysis::VertexZ(){return mVertexZ;}

#endif
