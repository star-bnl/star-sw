/***************************************************************************
 *
 * $Id: StDedxPidTraits.h,v 2.4 1999/11/29 17:07:27 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPidTraits.h,v $
 * Revision 2.4  1999/11/29 17:07:27  ullrich
 * Moved method() from StTrackPidTraits to StDedxPidTraits.cxx
 *
 * Revision 2.4  1999/11/29 17:07:27  ullrich
 * Moved method() from StTrackPidTraits to StDedxPidTraits.cxx
 *
 * Revision 2.3  1999/11/23 15:56:25  ullrich
 * Added clone() method. Was pure virtual.
 *
 * Revision 2.2  1999/11/16 14:11:41  ullrich
 * Changed variance to sigma.
 *
 * Revision 2.1  1999/10/13 19:42:58  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StDedxPidTraits_hh
#define StDedxPidTraits_hh
#include "StTrackPidTraits.h"

class StDedxPidTraits : public StTrackPidTraits {
public:
    StDedxPidTraits();
    StDedxPidTraits(StDetectorId, Short_t,
                    UShort_t, Float_t , Float_t);
    StDedxPidTraits(const dst_dedx_st&);
    // StDedxPidTraits(const StDedxPidTraits&);            use default
    // StDedxPidTraits& operator=(const StDedxPidTraits&); use default
    virtual ~StDedxPidTraits();
    
    StDedxMethod method() const;
    Short_t      encodedMethod() const;
    UShort_t     numberOfPoints() const;
    Float_t      mean() const;
    Float_t      sigma() const;
    
protected:
    UShort_t mNumberOfPoints;
    Float_t  mDedx;
    Float_t  mSigma;
    Short_t  mMethod;
    
    StObject* clone();
    ClassDef(StDedxPidTraits,1)
};
#endif
