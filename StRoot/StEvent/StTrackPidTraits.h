/***************************************************************************
 *
 * $Id: StTrackPidTraits.h,v 2.0 1999/10/12 18:43:07 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.h,v $
 * Revision 2.0  1999/10/12 18:43:07  ullrich
 * Completely Revised for New Version
 *
 * Revision 2.2  1999/11/15 18:48:28  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.1  1999/10/28 22:27:52  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:07  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StTrackPidTraits_hh
#define StTrackPidTraits_hh
#include "StObject.h"
#include "StEnumerations.h"

class dst_dedx_st;

class StTrackPidTraits : public StObject {
public:
    StTrackPidTraits();
    StTrackPidTraits(StDetectorId, Short_t);
    StTrackPidTraits(const dst_dedx_st&);
    // StTrackPidTraits(const StTrackPidTraits&);            use default
    Short_t method() const;
    Short_t detector() const;
    StDedxMethod method() const;
    

protected:
    Short_t mDetectorId;
    Short_t mMethod;

    virtual StObject* clone() = 0;
    ClassDef(StTrackPidTraits,1)
};
#endif
