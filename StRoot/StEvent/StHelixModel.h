/***************************************************************************
 *
 * $Id: StHelixModel.h,v 2.5 2001/07/17 22:23:30 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHelixModel.h,v $
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
    StHelixModel(const dst_track_st&);
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
     
    StTrackGeometry*       copy() const;     // virtual constructor

protected:
    StObject*      clone() const;
    
private:
    StTrackModel   mModel;
    Short_t        mCharge;
    Float_t        mPsi;
    Float_t        mCurvature;
    Float_t        mDipAngle;
    StThreeVectorF mOrigin;
    StThreeVectorF mMomentum;
    Short_t        mHelicity;
    
    ClassDef(StHelixModel,2)
};

#endif
