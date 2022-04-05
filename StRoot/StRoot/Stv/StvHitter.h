/// \File StvHitter.h
/// \author Victor Perev 04/2010
#ifndef StvHitter_HH
#define StvHitter_HH
#include "TNamed.h"
#include "StEvent/StEnumerations.h"
#include "StvStl.h"


class StvSeedFinder;
class StvHit;
class StHitPlane;
class StvNodePars;
class StMultiKeyMap;
class StMultiKeyMapIter;
class StvNodePars;
class StvFitErrs;
typedef float Mtx33F_t[3][3];

/// \class StvHitter, local hit provider
class StvHitter 
{
public:
  StvHitter();
 ~StvHitter();
void Reset();
const StvHits *GetHits(const StvNodePars *np, const StvFitErrs *ne,const float gate[2]);
const StHitPlane* GetHitPlane() const;
     StDetectorId GetDetId()    const;
private:
private:
const    StHitPlane 	*mHitPlane;
const StMultiKeyMap     *mHitMap;
const         float     *mOrg; 	//xyz of origine of hit plane
const      Mtx33F_t    	*mDir;	//orts of hit plane, local x,y,x orts
StMultiKeyMapIter	*fMultiIter;

StvHits mHits;
public:
StvSeedFinder *mSF;
};

#endif
