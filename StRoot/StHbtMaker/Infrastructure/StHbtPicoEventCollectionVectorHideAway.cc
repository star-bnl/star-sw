/***************************************************************************
 *
 * $Id: StHbtPicoEventCollectionVectorHideAway.cc,v 1.1 2000/07/16 21:44:11 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: StHbtPicoEventCollectionVectorHideAway.cc,v $
 * Revision 1.1  2000/07/16 21:44:11  laue
 * Collection and analysis for vertex dependent event mixing
 *
 *
 **************************************************************************/
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVectorHideAway.hh"

// -----------------------------------
StHbtPicoEventCollectionVectorHideAway::StHbtPicoEventCollectionVectorHideAway(int bins, double min, double max) {
    mBins = bins;
    mMin  = min;
    mMax  = max;
    mStep = (max-min)/bins;
    //mCollectionVector = new StHbtPicoEventCollectionVector();
    mCollection = 0;
    for ( int i=0; i<mBins; i++) {
	mCollection = new StHbtPicoEventCollection();
	mCollectionVector.push_back(mCollection);
    }
}
// -----------------------------------
StHbtPicoEventCollection* StHbtPicoEventCollectionVectorHideAway::PicoEventCollection(int bin) { 
  //cout << " StHbtPicoEventCollectionVectorHideAway::PicoEventCollection(int bin) - bin=" << bin << endl;
  if ( bin<0 || bin >= mBins) return 0;
  return mCollectionVector[bin]; 
}
// -----------------------------------
StHbtPicoEventCollection* StHbtPicoEventCollectionVectorHideAway::PicoEventCollection(double x) { 
  return PicoEventCollection( (int)floor( (x-mMin)/mStep )  );
}

