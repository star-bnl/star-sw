/***************************************************************************
 *
 * $Id: kaonPid.hh,v 1.1 2000/10/13 19:26:21 ullrich Exp $
 *
 * Author: Thomas Ullrich, Oct 1999
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: kaonPid.hh,v $
 * Revision 1.1  2000/10/13 19:26:21  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef kaonPid_hh
#define kaonPid_hh
#include "StEventTypes.h"

class kaonPid : public StPidAlgorithm {
public:
    kaonPid();
    
    StParticleDefinition*  operator() (const StTrack&, const StSPtrVecTrackPidTraits&);

    const  StDedxPidTraits* traits() const;

private:
    const StDedxPidTraits*        mTraits;       //!
    const StTrack*                mTrack;        //!
};
#endif


