/***********************************************************************
 * $Id: StRichMomentumTransform.cxx,v 1.2 2000/03/17 14:54:51 lasiuk Exp $
 *
 * Author: brian made this on Jan 27, 2000
 *
 ***********************************************************************
 * Description:
 *
 ***********************************************************************
 *
 * $Log: StRichMomentumTransform.cxx,v $
 * Revision 1.2  2000/03/17 14:54:51  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.1  2000/03/12 22:19:26  lasiuk
 * Initial Revision
 *
 ***********************************************************************/
#include "StRichMomentumTransform.h"

StRichMomentumTransform* StRichMomentumTransform::mInstance = 0;

StRichMomentumTransform*
StRichMomentumTransform::getTransform()
{
    if(!mInstance) {
	cout << "StRichMomentumTransform::getTransform()\n";
	cout << "\tERROR:\n";
	cout << "\tCannot call without a Geometry Database Existing:\n";
    }
    return mInstance;
}

StRichMomentumTransform*
StRichMomentumTransform::getTransform(StRichGeometryDbInterface* geo)
{
    if(!mInstance) {
	mInstance = new StRichMomentumTransform(geo);
    }

    return mInstance;
}


StRichMomentumTransform::StRichMomentumTransform()
{/* nopt */}

StRichMomentumTransform::StRichMomentumTransform(StRichGeometryDbInterface* geo)
    : mGeomDb(geo)
{
    // Survey
    mCosB = cos(mGeomDb->inclinationAngle());
    mSinB = sin(mGeomDb->inclinationAngle());
}

StRichMomentumTransform::~StRichMomentumTransform()
{
    delete mInstance;
}

void StRichMomentumTransform::globalMomentum(const StThreeVector<double>& a, StThreeVector<double>& b) const
{   
    //  ( x_g )     ( cos Þ   -sin Þ    0  ) ( y_l )
    //  ( y_g )  =  ( sin Þ    cos Þ    0  ) ( z_l )
    //  ( z_g )     (  0        0       1  ) ( x_l )
    //
    // where:
    // Þ = inclinationAngle (shift-alt p)
    //
    // Allow additional parameters from survey


    b.setX( mCosB*a.y() - mSinB*a.z() );
    b.setY( mSinB*a.y() + mCosB*a.z() );
    b.setZ(a.x());
}

void StRichMomentumTransform::localMomentum(const StThreeVector<double>& a, StThreeVector<double>& b) const
{
    // Inverse of above transformation!

    // (  cos Þ    sin Þ    0  ) ( x_g )     ( 1   0    0  ) ( y_l )
    // ( -sin Þ    cos Þ    0  ) ( y_g )  =  ( 0   1    0  ) ( z_l )
    // (   0        0       1  ) ( z_g )     ( 0   0    1  ) ( x_l )
    //
    // where:
    // Þ = inclinationAngle (shift-alt p)
    //
    // Allow additional parameters from survey

    b.setX(a.z());
    b.setY( mCosB*(a.x()) + mSinB*(a.y()) );
    b.setZ(-mSinB*(a.x()) + mCosB*(a.y()) );
}
