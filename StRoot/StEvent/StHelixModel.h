/***************************************************************************
 *
 * $Id: StHelixModel.h,v 2.2 1999/10/28 22:25:45 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHelixModel.h,v $
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
    StHelixModel(Short_t q, Float_t psi, Float_t c, Float_t dip,
                 const StThreeVectorF& o, const StThreeVectorF& p);
    StHelixModel(const dst_track_st&);
    // StHelixModel(const StHelixModel&);            use default
    // StHelixModel& operator=(const StHelixModel&); use default
    ~StHelixModel();

    StTrackModel          model() const;
    Short_t               charge() const;
    Double_t              curvature() const;
    Double_t              psi() const;
    Double_t              dipAngle() const;
    const StThreeVectorF& origin() const;
    const StThreeVectorF& momentum() const;
    StPhysicalHelixD      helix() const;

    StTrackGeometry*      copy() const;     // virtual constructor

protected:
    StObject*      clone();
    
private:
    StTrackModel   mModel;
    Short_t        mCharge;
    Float_t        mPsi;
    Float_t        mCurvature;
    Float_t        mDipAngle;
    StThreeVectorF mOrigin;
    StThreeVectorF mMomentum;
    
    ClassDef(StHelixModel,1)
};

#endif
