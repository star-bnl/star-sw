/***************************************************************************
 *
 * $Id: StHbtPicoEventCollectionVectorHideAway.hh,v 1.1 2000/07/16 21:44:11 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: StHbtPicoEventCollectionVectorHideAway.hh,v $
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

#if !defined(ST_NO_NAMESPACES)
using std::vector;
using std::list;
#endif

class StHbtPicoEventCollectionVectorHideAway {
public:
    StHbtPicoEventCollectionVectorHideAway(int bins=10, double min=-10., double max=10.);
    StHbtPicoEventCollection* PicoEventCollection(int);
    StHbtPicoEventCollection* PicoEventCollection(double);
private:
    int mBins;
    double mMin;
    double mMax;
    double mStep;
    StHbtPicoEventCollection* mCollection;
    StHbtPicoEventCollectionVector mCollectionVector;
};

#endif
