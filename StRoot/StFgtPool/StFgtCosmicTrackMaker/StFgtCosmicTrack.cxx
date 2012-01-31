/***************************************************************************
 *
 * $Id: StFgtCosmicTrack.cxx,v 1.1 2012/01/31 23:25:35 avossen Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 10 2011 
 *
 ***************************************************************************
 *
 * Description:  container class for cosmic tracks from teststand
 *
 ***************************************************************************
 *
 * $Log: StFgtCosmicTrack.cxx,v $
 * Revision 1.1  2012/01/31 23:25:35  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.2  2011/11/25 20:24:59  ckriley
 * now will look at all possible point combinations for tracks and pick the best one
 *
 * Revision 1.1  2011/11/01 18:49:59  sgliske
 * moved from StEvent/StFgtEvent to StFgtCosmicTrackMaker
 *
 * Revision 1.8  2011/10/26 19:43:30  ckriley
 * fix
 *
 * Revision 1.5  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#include "StObject.h"
#include "StFgtCosmicTrack.h"

// constructors
StFgtCosmicTrack::StFgtCosmicTrack() : mKey(-1), mA(1), mB(1),
                                       mX_0(0), mY_0(0),
                                       mVarX(1),mVarY(1),
                                       mChi2(1), mHitX(0),
                                       mHitY(0), mIsTrack(0) { /* */ };

StFgtCosmicTrack::StFgtCosmicTrack(Short_t key, Float_t a, Float_t b, Float_t x_0, Float_t y_0, Float_t varX, Float_t varY, Float_t chi2, Float_t hitX, Float_t hitY, Bool_t isTrack) : 
          mKey(key), mA(a), mB(b),
          mX_0(x_0), mY_0(y_0),  
          mVarX(varX), mVarY(varY),
          mChi2(chi2), mHitX(hitX),
          mHitY(hitY), mIsTrack(isTrack)  { /* */ };

// copy constructor
StFgtCosmicTrack::StFgtCosmicTrack( const StFgtCosmicTrack& rhs ) : 
                       mKey(rhs.mKey), mA(rhs.mA), mB(rhs.mB),
                       mX_0(rhs.mX_0), mY_0(rhs.mY_0),
                       mVarX(rhs.mVarX), mVarY(rhs.mVarY),
                       mChi2(rhs.mChi2), mHitX(rhs.mHitX),
                       mHitY(rhs.mHitY), mIsTrack(rhs.mIsTrack) { /* */ };

// equals operator
StFgtCosmicTrack& StFgtCosmicTrack::operator=( const StFgtCosmicTrack& rhs ){
   mKey  = rhs.mKey;
   mA    = rhs.mA;
   mB    = rhs.mB;
   mX_0  = rhs.mX_0;
   mY_0  = rhs.mY_0;
   mVarX = rhs.mVarX;
   mVarY = rhs.mVarY;
   mChi2 = rhs.mChi2;
   mHitX = rhs.mHitX;
   mHitY = rhs.mHitY;
   mIsTrack = rhs.mIsTrack;

   return *this;
};

ClassImp(StFgtCosmicTrack);
