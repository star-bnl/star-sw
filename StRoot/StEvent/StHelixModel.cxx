/***************************************************************************
 *
 * $Id: StHelixModel.cxx,v 2.3 2000/03/17 14:52:23 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHelixModel.cxx,v $
 * Revision 2.3  2000/03/17 14:52:23  ullrich
 * Method helix() now checks for q=0 and sets
 * curvature = 0 (which it should be anyhow).
 *
 * Revision 2.2  1999/10/28 22:25:42  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:49  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHelixModel.h"
#include "StThreeVectorF.hh"
#include "tables/St_dst_track_Table.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

ClassImp(StHelixModel)

static const char rcsid[] = "$Id: StHelixModel.cxx,v 2.3 2000/03/17 14:52:23 ullrich Exp $";

StHelixModel::StHelixModel() : mModel(helixModel)
{
    mPsi = 0;
    mCurvature = 0;
    mDipAngle = 0;
    mCharge = 0;
}

StHelixModel::StHelixModel(Short_t q, Float_t psi, Float_t c, Float_t dip,
                           const StThreeVectorF& o, const StThreeVectorF& p)
    : mModel(helixModel),
      mCharge(q),
      mPsi(psi),
      mCurvature(c),
      mDipAngle(dip),
      mOrigin(o),
      mMomentum(p)
{/* noop */}

StHelixModel::StHelixModel(const dst_track_st& t) :  mModel(helixModel)
{
    mPsi       = t.psi*degree;
    mCurvature = t.curvature;
    mDipAngle  = atan(t.tanl);
    mCharge    = t.icharge;
    mOrigin.setX(t.r0*cos(t.phi0*degree));
    mOrigin.setY(t.r0*sin(t.phi0*degree));
    mOrigin.setZ(t.z0);
    double pt = mCurvature > 0 ? 1./t.invpt : 0;
    double pz = pt*t.tanl;
    mMomentum.setX(pt*cos(mPsi));
    mMomentum.setY(pt*sin(mPsi));
    mMomentum.setZ(pz);
}

StHelixModel::~StHelixModel() { /* noop */ }

StTrackGeometry*
StHelixModel::copy() const { return new StHelixModel(*this); }

StObject*
StHelixModel::clone() { return new StHelixModel(*this); }

StTrackModel
StHelixModel::model() const {return mModel;}

Short_t
StHelixModel::charge() const {return mCharge;}

Double_t
StHelixModel::curvature() const {return mCurvature;}

Double_t
StHelixModel::psi() const {return mPsi;}

Double_t
StHelixModel::dipAngle() const {return mDipAngle;}

const StThreeVectorF&
StHelixModel::origin() const {return mOrigin;}

const StThreeVectorF&
StHelixModel::momentum() const {return mMomentum;}

StPhysicalHelixD
StHelixModel::helix() const
{
    //
    //  No charge no curvature.
    //  Agreement is to use q=0 as equivalent
    //  to saying curvature = 0. It should be
    //  0 but we better make sure.
    //
    double curvature = mCurvature;
    if (mCharge == 0) curvature = 0;

    //
    //  h = -sign(q*B)
    //  Here we assume B > 0. Here we run into
    //  problems if STAR ever switches polarisation.
    //  For B=0, h is ill defined and we can use
    //  +1 or -1. Both work as long as the phase
    //  is calculated correctly. Here we use the
    //  +1 convention.
    //
    int h;
    if (mCharge == 0)
	h = 1;
    else if (mCharge > 0)
	h = -1;
    else
	h = 1;
    
    double phase = mPsi-h*pi/2;
    
    return StPhysicalHelixD(curvature,    // 1/cm
                            mDipAngle,    // radian
                            phase,        // radian
                            mOrigin,      // cm
                            h);
}
