/*!
 * \class StTrackPidTraits 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrackPidTraits.h,v 2.6 2002/02/22 22:56:53 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.h,v $
 * Revision 2.6  2002/02/22 22:56:53  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.5  2001/04/05 04:00:46  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/24 03:35:00  perev
 * clone() -> clone() const
 *
 * Revision 2.3  1999/11/29 17:07:32  ullrich
 * Moved method() from StTrackPidTraits to StDedxPidTraits.cxx
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
    StTrackPidTraits(StDetectorId);
    StTrackPidTraits(const dst_dedx_st&);
    // StTrackPidTraits(const StTrackPidTraits&);            use default
    // StTrackPidTraits& operator=(const StTrackPidTraits&); use default
    virtual ~StTrackPidTraits();
    
    short detector() const;

protected:
    Short_t mDetectorId;

    virtual StObject* clone() const = 0;
    ClassDef(StTrackPidTraits,1)
};
#endif
