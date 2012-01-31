/***************************************************************************
 *
 * $Id: StFgtCosmicTrack.h,v 1.1 2012/01/31 23:25:35 avossen Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 10 2011 
 *
 ***************************************************************************
 *
 * Description:  container class for cosmic tracks from teststand
 *
 ***************************************************************************
 *
 * $Log: StFgtCosmicTrack.h,v $
 * Revision 1.1  2012/01/31 23:25:35  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.3  2011/11/25 20:24:59  ckriley
 * now will look at all possible point combinations for tracks and pick the best one
 *
 * Revision 1.2  2011/11/09 20:56:58  ckriley
 * working version for new containers
 *
 * Revision 1.1  2011/11/01 18:49:59  sgliske
 * moved from StEvent/StFgtEvent to StFgtCosmicTrackMaker
 *
 * Revision 1.8  2011/10/26 19:44:28  ckriley
 * fix
 *
 * Revision 1.5  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_COSMIC_TRACK_H_
#define _ST_FGT_COSMIC_TRACK_H_

#include <vector>
#include "Stypes.h"

class StFgtCosmicTrack;
typedef std::vector< StFgtCosmicTrack > StFgtCosmicTrackVec;

class StFgtCosmicTrack {
 public:
   // constructors
   StFgtCosmicTrack();
   StFgtCosmicTrack( Short_t key, Float_t a=1, Float_t b=1, Float_t x_0=0, Float_t y_0=0, Float_t varX=1, Float_t varY=1, Float_t chi2=1, Float_t hitX=0, Float_t hitY=0, Bool_t isTrack=0 );
   StFgtCosmicTrack(const StFgtCosmicTrack&);
   StFgtCosmicTrack& operator=(const StFgtCosmicTrack&);

   // deconstructor
   ~StFgtCosmicTrack();

   // accessors
   Short_t getKey() const;
   Float_t getLineParameterA() const;
   Float_t getLineParameterB() const;
   Float_t getLineParameterX_0() const;
   Float_t getLineParameterY_0() const;
   Float_t getVarX() const;
   Float_t getVarY() const;
   Float_t getChiSquare() const;
   Float_t getHitX() const;
   Float_t getHitY() const;
   Float_t getIsTrack() const;

   // modifiers
   void setKey( Short_t key ); 

 protected:
   // data members
   Short_t mKey;
   Float_t mA;
   Float_t mB;
   Float_t mX_0;
   Float_t mY_0;
   Float_t mVarX; // dX
   Float_t mVarY; // dY
   Float_t mChi2;
   Float_t mHitX; // actual point on mid quad
   Float_t mHitY; // actual point on mid quad
   Bool_t  mIsTrack;

 private:   
   ClassDef(StFgtCosmicTrack,1);
}; 

// inline functions

// deconstructor
inline StFgtCosmicTrack::~StFgtCosmicTrack() { /* */ };

// accessors
inline Short_t StFgtCosmicTrack::getKey() const {
  return mKey;
};

inline Float_t StFgtCosmicTrack::getLineParameterA() const {
  return mA;
};

inline Float_t StFgtCosmicTrack::getLineParameterB() const {
  return mB;
};

inline Float_t StFgtCosmicTrack::getLineParameterX_0() const {
  return mX_0;
};

inline Float_t StFgtCosmicTrack::getLineParameterY_0() const {
  return mY_0;
};

inline Float_t StFgtCosmicTrack::getVarX() const {
  return mVarX;
};

inline Float_t StFgtCosmicTrack::getVarY() const {
  return mVarY;
};

inline Float_t StFgtCosmicTrack::getChiSquare() const {
  return mChi2;
};

inline Float_t StFgtCosmicTrack::getHitX() const {
  return mHitX;
};

inline Float_t StFgtCosmicTrack::getHitY() const {
  return mHitY;
};

inline Float_t StFgtCosmicTrack::getIsTrack() const {
  return mIsTrack;
};


// modifiers
inline void StFgtCosmicTrack::setKey( Short_t key ){
   mKey = key;
};

#endif
