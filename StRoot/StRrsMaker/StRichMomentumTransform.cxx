/***********************************************************************
 * $Id: StRichMomentumTransform.cxx,v 2.0 2000/08/09 16:17:01 gans Exp $
 *
 * Author: brian made this on Jan 27, 2000
 *
 ***********************************************************************
 * Description:
 *
 ***********************************************************************
 *
 * $Log: StRichMomentumTransform.cxx,v $
 * Revision 2.0  2000/08/09 16:17:01  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
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
  mLocalAngleZ = mGeomDb->localAngleZ();
  mLocalAngleY = mGeomDb->localAngleY();
  mLocalAngleX = mGeomDb->localAngleX();
  
  mLocalRotate[0][0] = 
    (cos(mLocalAngleZ)*sin(mLocalAngleY)*cos(mLocalAngleX) +
     sin(mLocalAngleZ)*sin(mLocalAngleX) )
    ;

  mLocalRotate[1][0] = 
    (sin(mLocalAngleZ)*sin(mLocalAngleY)*cos(mLocalAngleX) -
     cos(mLocalAngleZ)*sin(mLocalAngleX) )
    ;
  mLocalRotate[2][0] =
    (cos(mLocalAngleY)*cos(mLocalAngleX))
    ;
  
  mLocalRotate[0][1] =
    (cos(mLocalAngleZ)*cos(mLocalAngleY))
      ;
  
  mLocalRotate[1][1] = 
    (sin(mLocalAngleZ)*cos(mLocalAngleY))
    ;
  mLocalRotate[2][1] =  
    (-sin(mLocalAngleY))
    ;

  mLocalRotate[0][2] =
    (cos(mLocalAngleZ)*sin(mLocalAngleY)*sin(mLocalAngleX) -
     sin(mLocalAngleZ)*cos(mLocalAngleX))
    ;
  mLocalRotate[1][2] = 
    (sin(mLocalAngleZ)*sin(mLocalAngleY)*sin(mLocalAngleX) +
     cos(mLocalAngleZ)*cos(mLocalAngleX))
      ;
  mLocalRotate[2][2] =
    (cos(mLocalAngleY) * sin(mLocalAngleX));

}

StRichMomentumTransform::~StRichMomentumTransform()
{
    delete mInstance;
}

void StRichMomentumTransform::globalMomentum(const StThreeVector<double>& a, StThreeVector<double>& b) const
{   
  // This returns a local (x,y,z) in global coordinates;
  // Transformation is Rz*Ry*Rx*P(xyz->zxy)
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(a);
#endif

    double localVec[3];

    localVec[0] = a.x();
    localVec[1] = a.y();
    localVec[2] = a.z();

    double rotateVec[3];

    // Rotate and perm    
    {
      // Stupid CC4
      int i;
      
      for (i = 0; i<3; i++) {
	int j;
	rotateVec[i] = 0;
	for (j=0; j<3; j++) {
	  rotateVec[i] = rotateVec[i] + mLocalRotate[i][j] * localVec[j];
	}
      }
    }

    b.setX( rotateVec[0]);
    b.setY( rotateVec[1]);
    b.setZ( rotateVec[2]);
}

void StRichMomentumTransform::localMomentum(const StThreeVector<double>& a, StThreeVector<double>& b) const
{
  // This returns a local (x,y,z) in global coordinates;
  // Transformation is inverse of above transformation.
  // Transpose usable since rotations orthogonal.

    double globalVec[3];
    globalVec[0] = a.x();
    globalVec[1] = a.y();
    globalVec[2] = a.z();
    double rotateVec[3];

    // Rotate and perm    
    {
      // Stupid CC4
      int i;
      
      for (i = 0; i<3; i++) {
	int j;
	rotateVec[i] = 0;
	for (j=0; j<3; j++) {
	  rotateVec[i] = rotateVec[i] + mLocalRotate[j][i] * globalVec[j];
	}
      }
    }

    b.setX(rotateVec[0]);
    b.setY(rotateVec[1]); 
    b.setZ(rotateVec[2]);

}
