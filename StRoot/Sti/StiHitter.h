/// \File StiHitter.h
/// \author Victor Perev 04/2010
#ifndef StiHitter_HH
#define StiHitter_HH
#include "TNamed.h"
#include "StEvent/StEnumerations.h"
#include "StiStl.h"


class StiHit;
class StHitPlane;
class StiNodePars;
class StMultiKeyMap;
class StMultiKeyMapIter;
typedef float Mtx33F_t[3][3];

/// \class StiHitter, local hit provider
class StiHitter 
{
public:
  StiHitter();
 ~StiHitter();
void Reset();
const StiHits *GetHits(const StiNodePars *np, const float gate[2]);
const StHitPlane* GetHitPlane() const;
     StDetectorId GetDetId()    const;
private:
private:
const    StHitPlane 	*mHitPlane;
const StMultiKeyMap     *mHitMap;
const         float     *mOrg; 
const      Mtx33F_t    	*mDir;
StMultiKeyMapIter	*fMultiIter;

StiHits mHits;
};

#endif
