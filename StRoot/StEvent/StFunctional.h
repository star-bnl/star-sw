/***************************************************************************
 *
 * $Id: StFunctional.h,v 2.1 1999/10/13 19:43:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFunctional.h,v $
 * Revision 2.1  1999/10/13 19:43:15  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:15  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFunctional_hh
#define StFunctional_hh
#include "Rtypes.h"
#include "StArray.h"

class StTrack;
class StHit;
class StParticleDefinition;
class StTrackPidTraits;

struct StTrackFilter
{
    virtual Bool_t operator() (const StTrack*) = 0;
};

struct StHitFilter
{
    virtual Bool_t operator() (const StHit*) = 0;
};

struct StPidAlgorithm
{
    virtual StParticleDefinition* operator() (const StTrack&, const StSPtrVecTrackPidTraits&) = 0;
};

#endif
