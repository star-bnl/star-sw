/*!
 * \class StHelixModel 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StHelixModel.h,v 2.10 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHelixModel.h,v $
 * Revision 2.10  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.9  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.8  2003/04/09 17:59:39  genevb
 * Add setMomentum function
 *
 * Revision 2.7  2002/11/26 02:19:11  perev
 * StEventMaker ITTF modif
 *
 * Revision 2.6  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.5  2001/07/17 22:23:30  ullrich
 * Added helicity to track geometry.
 *
 * Revision 2.4  2001/04/05 04:00:37  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  2001/03/24 03:34:49  perev
 * clone() -> clone() const
 *
 * Revision 2.2  1999/10/28 22:25:45  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHelixModel_hh
#define StHelixModel_hh
#include "StTrackGeometry.h"
#include "StThreeVectorF.hh"

class StHelixModel : public StTrackGeometry {
public:
    StHelixModel();
    StHelixModel(short q, float psi, float c, float dip,
                 const StThreeVectorF& o, const StThreeVectorF& p, short h);
    // StHelixModel(const StHelixModel&);            use default
    // StHelixModel& operator=(const StHelixModel&); use default
    ~StHelixModel();

    StTrackModel           model() const;
    short                  charge() const;
    short                  helicity() const;
    double                 curvature() const;
    double                 psi() const;
    double                 dipAngle() const;
    const StThreeVectorF&  origin() const;
    const StThreeVectorF&  momentum() const;
    StPhysicalHelixD       helix() const;

    void setCharge(short);
    void setHelicity(short);
    void setCurvature(double);
    void setPsi(double);
    void setDipAngle(double);
    void setOrigin(const StThreeVectorF&);
    void setMomentum(const StThreeVectorF&);
     
    StTrackGeometry*       copy() const;     // virtual constructor
    
private:
    StTrackModel   mModel;
    Short_t        mCharge;
    Float_t        mPsi;
    Float_t        mCurvature;
    Float_t        mDipAngle;
    StThreeVectorF mOrigin;
    StThreeVectorF mMomentum;
    Short_t        mHelicity;
    
    ClassDef(StHelixModel,3)
};

#endif
