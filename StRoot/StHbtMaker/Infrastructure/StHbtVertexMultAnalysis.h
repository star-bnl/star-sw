/***************************************************************************
 *
 * $Id: StHbtVertexMultAnalysis.h,v 1.2 2002/11/03 16:37:43 magestro Exp $
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
 * $Log: StHbtVertexMultAnalysis.h,v $
 * Revision 1.2  2002/11/03 16:37:43  magestro
 * Moved StHbtPicoEventCollectionVectorHideAway object to StHbtAnalysis for circular event-mixing
 *
 * Revision 1.1  2001/11/11 18:34:13  laue
 * StHbtPicoEventCollectionVectorHideAway: updated for 3d grid
 * StHbtVertexMultAnalysis: new
 *
 *
 *
 **************************************************************************/

#ifndef StHbtVertexMultAnalysis_hh
#define StHbtVertexMultAnalysis_hh

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"        // base analysis class
#include <limits.h>

class StHbtVertexMultAnalysis : public StHbtAnalysis {

public:

  StHbtVertexMultAnalysis(unsigned int=10, double=-100., double=+100., unsigned int b=10, double=-1.e9, double=+1.e9);
  StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis&);  // copy constructor
  virtual void ProcessEvent(const StHbtEvent*);
  virtual ~StHbtVertexMultAnalysis();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done
  virtual unsigned int OverflowVertexZ() { return mOverFlowVertexZ;}
  virtual unsigned int UnderflowVertexZ() { return mUnderFlowVertexZ;}
  virtual unsigned int OverflowMult() { return mOverFlowMult;}
  virtual unsigned int UnderflowMult() { return mUnderFlowMult;}
protected:
  double mVertexZ[2];
  unsigned int mVertexZBins;
  unsigned int mOverFlowVertexZ;
  unsigned int mUnderFlowVertexZ;
  double mMult[2];
  unsigned int mMultBins;
  unsigned int mOverFlowMult;
  unsigned int mUnderFlowMult;
  
#ifdef __ROOT__
  ClassDef(StHbtVertexMultAnalysis, 0)
#endif
    
};

#endif
