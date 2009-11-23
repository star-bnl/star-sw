/***************************************************************************
 *
 * $Id: StHelixModel.cxx,v 2.12 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHelixModel.cxx,v $
 * Revision 2.12  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.11  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.10  2003/12/04 03:51:11  perev
 * Set small but non zero curvature
 *
 * Revision 2.9  2003/04/09 17:59:39  genevb
 * Add setMomentum function
 *
 * Revision 2.8  2001/07/21 18:04:02  ullrich
 * Added code to helix() in order to stay backwards compatible.
 *
 * Revision 2.7  2001/07/19 16:18:46  ullrich
 * Added missing method helicity().
 *
 * Revision 2.6  2001/07/17 22:23:30  ullrich
 * Added helicity to track geometry.
 *
 * Revision 2.5  2001/04/05 04:00:50  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/24 03:34:49  perev
 * clone() -> clone() const
 *
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
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

ClassImp(StHelixModel)

static const char rcsid[] = "$Id: StHelixModel.cxx,v 2.12 2009/11/23 16:34:06 fisyak Exp $";

StHelixModel::StHelixModel() : mModel(helixModel)
{
    mPsi = 0;
    mCurvature = 1.e-6;
    mDipAngle = 0;
    mCharge = 0;
    mHelicity = 0;
}

StHelixModel::StHelixModel(short q, float psi, float c, float dip,
                           const StThreeVectorF& o, const StThreeVectorF& p, short h)
    : mModel(helixModel),
      mCharge(q),
      mPsi(psi),
      mCurvature(c+1.e-10),
      mDipAngle(dip),
      mOrigin(o),
      mMomentum(p),
      mHelicity(h)
{/* noop */}

StHelixModel::~StHelixModel() { /* noop */ }

StTrackGeometry*
StHelixModel::copy() const { return new StHelixModel(*this); }

StTrackModel
StHelixModel::model() const {return mModel;}

short
StHelixModel::charge() const {return mCharge;}

short
StHelixModel::helicity() const {return mHelicity;}

double
StHelixModel::curvature() const {return mCurvature;}

double
StHelixModel::psi() const {return mPsi;}

double
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
    //  mHelicity is needed at this point. It is NOT
    //  filled from the constructor using the table
    //  but has to be provided separately.
    //
    //  For B=0, h is ill defined and we can use
    //  +1 or -1. Both work as long as the phase
    //  is calculated correctly. Here we use the
    //  +1 convention.
    //
    int h = mHelicity;

    //
    //  Need to stay backwards compatible. All we can do here
    //  is to assume B > 0. This is OK since the inverse field
    //  and the introduction of mHelicity happened at the same time.
    //
    if (h == 0) {
	if (mCharge == 0)
	    h = 1;
	else if (mCharge > 0)
	    h = -1;
	else
	    h = 1;
    }
    // end backwards compatibility fix

    
    if (mCharge == 0) h = 1;
    
    double phase = mPsi-h*pi/2;
    
    return StPhysicalHelixD(curvature,    // 1/cm
                            mDipAngle,    // radian
                            phase,        // radian
                            mOrigin,      // cm
                            h);
}

void
StHelixModel::setCharge(short val) { mCharge = val; }

void
StHelixModel::setHelicity(short val) { mHelicity = val; }

void
StHelixModel::setCurvature(double val) { mCurvature = val; }

void
StHelixModel::setPsi(double val) { mPsi = val; }

void
StHelixModel::setDipAngle(double val) { mDipAngle = val; }

void
StHelixModel::setOrigin(const StThreeVectorF& val) { mOrigin = val; }

void
StHelixModel::setMomentum(const StThreeVectorF& val) { mMomentum = val; }
