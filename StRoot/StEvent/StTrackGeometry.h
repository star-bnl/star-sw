/***************************************************************************
 *
 * $Id: StTrackGeometry.h,v 2.1 1999/10/13 19:44:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackGeometry.h,v $
 * Revision 2.1  1999/10/13 19:44:13  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:44:13  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTrackGeometry_hh
#define StTrackGeometry_hh
#include "StObject.h"
#include "StEnumerations.h"
#include "StPhysicalHelixD.hh"

class dst_track_st;
class StThreeVectorF;

class StTrackGeometry : public StObject {
public:
    StTrackGeometry();
    StTrackGeometry(const dst_track_st&);
    // StTrackGeometry(const StTrackGeometry&);             use default
    // StTrackGeometry & operator=(const StTrackGeometry&); use default
    virtual ~StTrackGeometry();

    virtual StTrackModel          model() const = 0;
    virtual Short_t               charge() const = 0;
    virtual Double_t              curvature() const = 0;
    virtual Double_t              psi() const = 0;
    virtual Double_t              dipAngle() const = 0;
    virtual const StThreeVectorF& origin() const = 0;
    virtual const StThreeVectorF& momentum() const = 0;
    virtual StPhysicalHelixD      helix() const = 0;

    virtual StTrackGeometry*      clone() const = 0;     // virtual constructor
    
    ClassDef(StTrackGeometry,1)
};

#endif
