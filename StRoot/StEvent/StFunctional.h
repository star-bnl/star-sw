/*!
 * \file StFunctional.h
 */
/***************************************************************************
 *
 * $Id: StFunctional.h,v 2.4 2002/02/22 22:56:48 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFunctional.h,v $
 * Revision 2.4  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:37  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  1999/10/28 22:25:30  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:15  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFunctional_hh
#define StFunctional_hh
#include "Rtypes.h"
#include "StContainers.h"

class StTrack;
class StHit;
class StParticleDefinition;
class StTrackPidTraits;

/*!
 * \struct StTrackFilter
 */
struct StTrackFilter
{
    virtual bool operator() (const StTrack*) = 0;
};

/*!
 * \struct StHitFilter
 */
struct StHitFilter
{
    virtual bool operator() (const StHit*) = 0;
};

/*!
 * \struct StPidAlgorithm
 */
struct StPidAlgorithm
{
    virtual StParticleDefinition* operator() (const StTrack&, const StSPtrVecTrackPidTraits&) = 0;
    virtual ~StPidAlgorithm() {}
};

#endif
