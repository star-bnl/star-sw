/***************************************************************************
 *
 * $Id: StTrackGeometry.h,v 2.5 2001/07/17 22:23:30 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackGeometry.h,v $
 * Revision 2.5  2001/07/17 22:23:30  ullrich
 * Added helicity to track geometry.
 *
 * Revision 2.4  2001/04/05 04:00:45  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  2001/03/24 03:35:00  perev
 * clone() -> clone() const
 *
 * Revision 2.2  1999/10/28 22:27:41  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
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

    virtual StTrackModel           model() const = 0;
    virtual short                  charge() const = 0;
    virtual short                  helicity() const = 0;
    virtual double                 curvature() const = 0;
    virtual double                 psi() const = 0;
    virtual double                 dipAngle() const = 0;
    virtual const StThreeVectorF&  origin() const = 0;
    virtual const StThreeVectorF&  momentum() const = 0;
    virtual StPhysicalHelixD       helix() const = 0;

    virtual void setCharge(short) = 0;
    virtual void setHelicity(short) = 0;
    virtual void setCurvature(double) = 0; 
    virtual void setPsi(double) = 0;
    virtual void setDipAngle(double) = 0;
    virtual void setOrigin(const StThreeVectorF&) = 0;
    
    virtual StTrackGeometry*       copy() const = 0;     // virtual constructor

protected:
    virtual StObject*  clone() const = 0;     // virtual constructor used in StArray
    ClassDef(StTrackGeometry,2)
};

#endif
