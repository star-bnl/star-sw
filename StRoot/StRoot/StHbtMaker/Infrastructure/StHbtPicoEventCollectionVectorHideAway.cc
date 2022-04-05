/***************************************************************************
 *
 * $Id: StHbtPicoEventCollectionVectorHideAway.cc,v 1.3 2002/11/01 20:45:53 magestro Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: StHbtPicoEventCollectionVectorHideAway.cc,v $
 * Revision 1.3  2002/11/01 20:45:53  magestro
 * Fixed bug in 3rd dimension of event collection vector, probably never encountered
 *
 * Revision 1.2  2001/11/11 18:34:13  laue
 * StHbtPicoEventCollectionVectorHideAway: updated for 3d grid
 * StHbtVertexMultAnalysis: new
 *
 * Revision 1.1  2000/07/16 21:44:11  laue
 * Collection and analysis for vertex dependent event mixing
 *
 *
 **************************************************************************/
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVectorHideAway.hh"

// -----------------------------------
StHbtPicoEventCollectionVectorHideAway::StHbtPicoEventCollectionVectorHideAway(int bx, double lx, double ux,
									       int by, double ly, double uy,
									       int bz, double lz, double uz) {
    mBinsx = bx;  mMinx = lx; mMaxx = ux;
    mBinsy = by;  mMiny = ly; mMaxy = uy;
    mBinsz = bz;  mMinz = lz; mMaxz = uz;

    mBinsTot = mBinsx * mBinsy * mBinsz;
    mStepx=0;  mStepx = (mMaxx-mMinx)/mBinsx;
    mStepy=0;  mStepy = (mMaxy-mMiny)/mBinsy;
    mStepz=0;  mStepz = (mMaxz-mMinz)/mBinsz;
    

    //mCollectionVector = new StHbtPicoEventCollectionVector();
    mCollection = 0;
    for ( int i=0; i<mBinsTot; i++) {
	mCollection = new StHbtPicoEventCollection();
	mCollectionVector.push_back(mCollection);
    }
}
// -----------------------------------
StHbtPicoEventCollection* StHbtPicoEventCollectionVectorHideAway::PicoEventCollection(int ix, int iy, int iz) { 
  if ( ix<0 || ix >= mBinsx) return 0;
  if ( iy<0 || iy >= mBinsy) return 0;
  if ( iz<0 || iz >= mBinsz) return 0;
  int bin = ix + iy*mBinsx + iz*mBinsy*mBinsx; 
  cout << " StHbtPicoEventCollectionVectorHideAway::PicoEventCollection(...) - bin(ix,iy,iz): ";
  cout << bin << "(" << ix <<"," << iy << "," << iz <<")" << endl;
  return mCollectionVector[bin]; 
}
// -----------------------------------
StHbtPicoEventCollection* StHbtPicoEventCollectionVectorHideAway::PicoEventCollection(double x, double y, double z) {
  int ix,iy,iz;
  ix=0;iy=0;iz=0;

  ix = (int)floor( (x-mMinx)/mStepx );
  iy = (int)floor( (y-mMiny)/mStepy );
  iz = (int)floor( (z-mMinz)/mStepz );

  return PicoEventCollection( ix,iy,iz );
}

