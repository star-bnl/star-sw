/***************************************************************************
 *
 * $Id: StHbtPicoEventCollectionVectorHideAway.hh,v 1.2 2001/11/11 18:34:13 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: StHbtPicoEventCollectionVectorHideAway.hh,v $
 * Revision 1.2  2001/11/11 18:34:13  laue
 * StHbtPicoEventCollectionVectorHideAway: updated for 3d grid
 * StHbtVertexMultAnalysis: new
 *
 * Revision 1.1  2000/07/16 21:44:11  laue
 * Collection and analysis for vertex dependent event mixing
 *
 *
 **************************************************************************/
#ifndef StHbtPicoEventCollectionVectorHideAway_hh
#define StHbtPicoEventCollectionVectorHideAway_hh
#include "StHbtMaker/Infrastructure/StHbtPicoEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVector.hh"
#include <vector>
#include <list>
#include <limits.h>

#if !defined(ST_NO_NAMESPACES)
using std::vector;
using std::list;
#endif

class StHbtPicoEventCollectionVectorHideAway {
public:
  StHbtPicoEventCollectionVectorHideAway(int bx=1, double lx=DBL_MIN, double ux=DBL_MAX,
					 int by=1, double ly=DBL_MIN, double uy=DBL_MAX,
					 int bz=1, double lz=DBL_MIN, double uz=DBL_MAX);
  StHbtPicoEventCollection* PicoEventCollection(int, int, int);
  StHbtPicoEventCollection* PicoEventCollection(double x, double y=0, double z=0);
private:
  int mBinsTot;
  int mBinsx,mBinsy,mBinsz;
  double mMinx,mMiny,mMinz;
  double mMaxx,mMaxy,mMaxz;
  double mStepx,mStepy,mStepz;
  StHbtPicoEventCollection* mCollection;
  StHbtPicoEventCollectionVector mCollectionVector;
};

#endif
