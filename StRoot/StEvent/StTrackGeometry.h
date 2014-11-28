/*!
 * \class StTrackGeometry 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrackGeometry.h,v 2.11 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackGeometry.h,v $
 * Revision 2.11  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.10  2005/07/06 19:00:52  fisyak
 * Add include of StThreeVectorD.hh
 *
 * Revision 2.9  2004/07/15 16:36:26  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.8  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.7  2003/04/09 17:59:39  genevb
 * Add setMomentum function
 *
 * Revision 2.6  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
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
#include "StThreeVectorF.hh"

class StTrackGeometry : public StObject {
public:
    StTrackGeometry();
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
    virtual void setMomentum(const StThreeVectorF&) = 0;
    
    virtual StTrackGeometry*       copy() const = 0;     // virtual constructor
    int     bad() const;

    ClassDef(StTrackGeometry,2)
};

#endif
