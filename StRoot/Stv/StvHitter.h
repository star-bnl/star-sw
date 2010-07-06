/// \File StvHitter.h
/// \author Victor Perev 04/2010
#ifndef StvHitter_HH
#define StvHitter_HH
#include "TNamed.h"
#include "StEvent/StEnumerations.h"
#include "StvStl.h"


class StvHit;
class StHitPlane;
class StvNodePars;
class StMultiKeyMap;
class StMultiKeyMapIter;
typedef float Mtx33F_t[3][3];

/// \class StvHitter, local hit provider
class StvHitter 
{
public:
  StvHitter();
 ~StvHitter();
void Reset();
const StvHits *GetHits(const StvNodePars *np, const float gate[2]);
const StHitPlane* GetHitPlane() const;
     StDetectorId GetDetId()    const;
private:
private:
const    StHitPlane 	*mHitPlane;
const StMultiKeyMap     *mHitMap;
const         float     *mOrg; 
const      Mtx33F_t    	*mDir;
StMultiKeyMapIter	*fMultiIter;

StvHits mHits;
};

#endif
