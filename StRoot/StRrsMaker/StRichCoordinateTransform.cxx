/***********************************************************************
 * $Id: StRichCoordinateTransform.cxx,v 2.0 2000/08/09 16:16:59 gans Exp $
 *
 * Author: brian made this on Jan 27, 2000
 *
 ***********************************************************************
 * Description:
 *
 ***********************************************************************
 *
 * $Log: StRichCoordinateTransform.cxx,v $
 * Revision 2.0  2000/08/09 16:16:59  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.5  2000/03/17 14:54:17  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.4  2000/03/12 22:20:24  lasiuk
 * make into a singleton class
 * incliniation angle stored as data members
 *
 * Revision 1.3  2000/02/24 18:44:14  lasiuk
 * coerce quadrant bounds limits
 *
 * Revision 1.2  2000/02/08 23:49:18  lasiuk
 * rm fpad from routine which is not returned
 *
 * Revision 1.1  2000/02/08 16:34:03  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 ***********************************************************************/
#include "StRichCoordinateTransform.h"
#include "StRichOtherAlgorithms.h" // for nearestInteger
StRichCoordinateTransform* StRichCoordinateTransform::mInstance = 0;

StRichCoordinateTransform*
StRichCoordinateTransform::getTransform()
{
    if(!mInstance) {
	cout << "StRichCoordinateTransform::getTransform()\n";
	cout << "\tERROR:\n";
	cout << "\tCannot call without a Geometry Database Existing:\n";
    }
    return mInstance;
}

StRichCoordinateTransform*
StRichCoordinateTransform::getTransform(StRichGeometryDbInterface* geo)
{
    if(!mInstance) {
	mInstance = new StRichCoordinateTransform(geo);
    }

    return mInstance;
}


StRichCoordinateTransform::StRichCoordinateTransform()
{/* nopt */}

StRichCoordinateTransform::StRichCoordinateTransform(StRichGeometryDbInterface* geo)
    : mGeomDb(geo)
{
    mNumberOfPadsInAQuadrantRow = mGeomDb->numberOfPadsInAQuadrantRow();
    mNumberOfRowsInAQuadrantColumn = mGeomDb->numberOfRowsInAQuadrantColumn();
    mPadPitch = mGeomDb->padPitch();
    mRowPitch = mGeomDb->rowPitch();
    mXGap     = mGeomDb->quadrantGapInX();
    mYGap     = mGeomDb->quadrantGapInY();
    
    // For Bounds check of quadrant
//      mXQBound = -(mNumberOfRowsInAQuadrantColumn+1.)*mRowPitch/2;
//      mZQBound = -(mNumberOfPadsInAQuadrantRow+1.)*mPadPitch/2.;
    mXQBound = -(mNumberOfPadsInAQuadrantRow)*mPadPitch/2.;
    mYQBound = -(mNumberOfRowsInAQuadrantColumn)*mRowPitch/2;

    // For Bounds check of local
    mXLBound = -(mGeomDb->quadrantGapInX()/2. + (mNumberOfPadsInAQuadrantRow+.5)*mPadPitch);
    mYLBound = -(mGeomDb->quadrantGapInY()/2. + (mNumberOfRowsInAQuadrantColumn+.5)*mRowPitch);

    // Survey:  
    // Origin of Local in Global in cylindrical
    mLocalOriginR = mGeomDb->localOriginR();
    mLocalOriginAngle = mGeomDb->localOriginAngle();
    mLocalOriginZ = mGeomDb->localOriginZ();

    mLocalTrans[0] = mLocalOriginR * cos(mLocalOriginAngle);
    mLocalTrans[1] = mLocalOriginR * sin(mLocalOriginAngle);
    mLocalTrans[2] = mLocalOriginZ;

    
    // Inclination in "Euler-like" angles.
    // Rotation to get Local in Global is AngleZ*AngleY*AngleX*Permutation
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

StRichCoordinateTransform::~StRichCoordinateTransform()
{
    delete mInstance;
}

//
// Public Operators
//
// Raw --> Quadrant
void
StRichCoordinateTransform::operator()(const StRichRawCoordinate& a, StRichQuadrantCoordinate& b)
{
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int rawStatus = checkRawBounds(a);
#endif

    b.setQuadrant(whichQuadrant(a));

    b.position().setX(pad2QuadX(a.pad(), b.quadrant()));
    b.position().setY(row2QuadY(a.row(), b.quadrant()));
    b.position().setZ(0);

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int quadStatus = checkQuadrantBounds(b);
#endif
}

void
StRichCoordinateTransform::operator()(const StRichQuadrantCoordinate& a, StRichRawCoordinate& b)
{
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int quadStatus = checkQuadrantBounds(a);
#endif

    b.setPad(quadX2Pad(a.position().x(), a.quadrant()));
    b.setRow(quadY2Row(a.position().y(), a.quadrant()));

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int rawStatus = checkRawBounds(b);
#endif
}

//     Rich Quadrant <-->  Rich Local
void
StRichCoordinateTransform::operator()(const StRichQuadrantCoordinate& a, StRichLocalCoordinate& b)
{
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int quadStatus = checkQuadrantBounds(a);
#endif

    b.position().setX(a.position().x()+mGeomDb->quadrantXOrigin(a.quadrant()));
    b.position().setY(a.position().y()+mGeomDb->quadrantYOrigin(a.quadrant()));
    b.position().setZ(a.position().z());

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(b);
#endif
}

void
StRichCoordinateTransform::operator()(const StRichLocalCoordinate& a, StRichQuadrantCoordinate& b)
{
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(a);
#endif

    b.setQuadrant( whichQuadrant(a) );
    b.position().setX( a.position().x()-mGeomDb->quadrantXOrigin(b.quadrant()) );
    b.position().setY( a.position().y()-mGeomDb->quadrantYOrigin(b.quadrant()) );
    b.position().setZ( a.position().z() );

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int quadStatus = checkQuadrantBounds(b);
#endif
}
    
//     Rich Local    <-->  STAR Global Coordinate
void
StRichCoordinateTransform::operator()(const StRichLocalCoordinate& a, StGlobalCoordinate& b)
{   
  // This returns a local (x,y,z) in global coordinates;
  // Transformation is Rz*Ry*Rx*P(xyz->zxy) + Translation
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(a);
#endif

    double localVec[3];

    localVec[0] = a.position().x();
    localVec[1] = a.position().y();
    localVec[2] = a.position().z();

    double rotateVec[3];

    
    // Rotate and perm    
    {
      // Stupid CC4
      int i;
      
      for (i = 0; i<3; i++) {
	rotateVec[i] = 0.;
	int j;
	for (j=0; j<3; j++) {
	  rotateVec[i] = rotateVec[i] + mLocalRotate[i][j] * localVec[j];
	}
      }
    }

   
    // Translate
    double transVec[3];
    {
      int i;
      for (i=0; i< 3; i++) {
	transVec[i] = mLocalTrans[i] + rotateVec[i];
      }
    }

    b.position().setX( transVec[0]);
    b.position().setY( transVec[1]);
    b.position().setZ( transVec[2]);
}

void
StRichCoordinateTransform::operator()(const StGlobalCoordinate& a, StRichLocalCoordinate& b)
{
  // This returns a global (x,y,z) in local coordinates;
  // Transformation is inverse of above transformation.
  // Transpose usable since rotations orthogonal.
 

    double globalVec[3];
    globalVec[0] = a.position().x();
    globalVec[1] = a.position().y();
    globalVec[2] = a.position().z();

    // Translate
    double transVec[3];
    {
      int i;
      for (i=0; i< 3; i++) {
	transVec[i] = globalVec[i] - mLocalTrans[i];
      }
    }
    double rotateVec[3];

    // Rotate and perm    
    {
      // Stupid CC4
      int i;
      
      for (i = 0; i<3; i++) {
	int j;
	rotateVec[i] = 0.;
	for (j=0; j<3; j++) {
	  rotateVec[i] = rotateVec[i] + mLocalRotate[j][i] * transVec[j];
	}
      }
    }

    b.position().setX(rotateVec[0]);
    b.position().setY(rotateVec[1]); 
    b.position().setZ(rotateVec[2]);


#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(b);
#endif

}

//
//     Raw Data      <-->  Rich Local
void
StRichCoordinateTransform::operator()(const StRichRawCoordinate& a, StRichLocalCoordinate& b)
{
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int rawStatus = checkRawBounds(a);
#endif

    StRichQuadrantCoordinate quad;
     this->operator()(a,quad);
     this->operator()(quad,b);

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(b);
#endif
}

void
StRichCoordinateTransform::operator()(const StRichLocalCoordinate& a, StRichRawCoordinate& b)
{
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(a);
#endif

    StRichQuadrantCoordinate quad;
    this->operator()(a,quad);
    this->operator()(quad,b);
    
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int rawStatus = checkRawBounds(b);
#endif
}

//     Rich Quadrant <-->  Global Coordinate
void
StRichCoordinateTransform::operator()(const StRichQuadrantCoordinate& a, StGlobalCoordinate& b)
{
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int quadStatus = checkQuadrantBounds(a);
#endif
    
    StRichLocalCoordinate local;
    this->operator()(a,local);
    this->operator()(local,b); 
}

void
StRichCoordinateTransform::operator()(const StGlobalCoordinate& a, StRichQuadrantCoordinate& b)
{
    StRichLocalCoordinate local;
    this->operator()(a,local);
    this->operator()(local,b);

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int quadStatus = checkQuadrantBounds(b);
#endif
}
    
//     Raw Data     <-->  Global Coordinate
void
StRichCoordinateTransform::operator()(const StRichRawCoordinate& a, StGlobalCoordinate& b)
{
#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int rawStatus = checkRawBounds(a);
#endif
    
    StRichQuadrantCoordinate quad;
    StRichLocalCoordinate    local;
    this->operator()(a,quad);
    this->operator()(quad,local);
    this->operator()(local,b); 
}

void
StRichCoordinateTransform::operator()(const StGlobalCoordinate& a, StRichRawCoordinate& b)
{
    StRichLocalCoordinate    local;
    StRichQuadrantCoordinate quad;
    this->operator()(a,local);
    this->operator()(local,quad);
    this->operator()(quad,b);

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int rawStatus = checkRawBounds(b);
#endif
}

/********************************************************************************
 * Below are the private routines which do the work
 * They are *not* accessible externally.  ONLY through the () operators
 *
 *******************************************************************************/
// Raw --> Quadrant
int
StRichCoordinateTransform::whichQuadrant(const StRichRawCoordinate& raw) const
{
    if(raw.row() < (mNumberOfRowsInAQuadrantColumn) ) {
	// in quad 1 or 2
	if(raw.column() < (mNumberOfPadsInAQuadrantRow) ) {
	    return 2;
	}
	else { 
	    return 1;
	}
    }
    else { // must be quad 3 or 4
	if(raw.column() < (mNumberOfPadsInAQuadrantRow) ) {
	    return 3;
	}
	else {
	    return 4;
	}
    }
}

int
StRichCoordinateTransform::whichQuadrant(const StRichLocalCoordinate& loc) const
{
    if(loc.position().x() > 0 ) {
	// in quad 1 or 4
	if(loc.position().y() > 0 ) {
	    return 1;
	}
	else { 
	    return 4;
	}
    }
    else { // must be quad 2 or 3
	if(loc.position().y() > 0 ) {
	    return 2;
	}
	else {
	    return 3;
	}
    }
}

double
StRichCoordinateTransform::row2QuadY(double row, int quad) const
{
    double pseudoRow = row;

    if(quad == 3 || quad == 4) {
	pseudoRow = (row - mNumberOfRowsInAQuadrantColumn);
    }

    return ( -(pseudoRow-(mNumberOfRowsInAQuadrantColumn/2.-0.5))*mRowPitch );
}
    
double
StRichCoordinateTransform::pad2QuadX(double pad, int quad) const
{
	
    double pseudoPad = pad;
    if(quad == 1 || quad == 4) {
	pseudoPad = (pad - mNumberOfPadsInAQuadrantRow);
    }

    return ( (pseudoPad-(mNumberOfPadsInAQuadrantRow/2.-0.5))*mPadPitch );
}

double
StRichCoordinateTransform::quadY2Row(double qx, int quad) const
{
	
    double rowOffset = 0;
    if(quad == 3 || quad == 4) {
	rowOffset = mNumberOfRowsInAQuadrantColumn;
	if(qx>=-mYQBound)
	    qx = -mYQBound;
    }
    else {
	if(qx<=mYQBound)
	    qx = mYQBound;
    }

    return (rowOffset + ((-qx/mRowPitch - .5) + mNumberOfRowsInAQuadrantColumn/2.));
}

double
StRichCoordinateTransform::quadX2Pad(double qz, int quad) const
{
    double padOffset = 0;
    if(quad == 1 || quad == 4) {
	padOffset = mNumberOfPadsInAQuadrantRow;
	if(qz<=mXQBound)
	    qz = mXQBound;
    }
    else {
	if(qz>=-mXQBound)
	    qz = -mXQBound;
    }
    return (padOffset + ((qz/mPadPitch - .5) + mNumberOfPadsInAQuadrantRow/2.));
}

// int
// StRichCoordinateTransform::nearestInteger(double x) const
// {
//     return ( static_cast<int>(x+.5) );
// }

//
/**************************************************************************************/
// These routines check the bounds of the values when the MACRO
//RICH_COORDINATE_BOUNDS_CHECK
// is defined
int StRichCoordinateTransform::checkRawBounds(const StRichRawCoordinate& raw) const
{
    if( ((nearestInteger(raw.pad()) < mGeomDb->numberOfPadsInARow())    &&
	 (nearestInteger(raw.pad()) >= 0) )                             &&
	((nearestInteger(raw.row()) < mGeomDb->numberOfRowsInAColumn()) &&
	 (nearestInteger(raw.row()) >= 0)) ) {
	return 0;
    }
    else {
	cout << "StRichCoordinateTransform::checkRawBounds()\n";
	cout << "\tWARNING:\n";
	cout << "\t0 <= #Pads < " <<  (mGeomDb->numberOfPadsInARow())
	     << " (" << nearestInteger(raw.pad()) << ") " << raw.pad() << endl;
	cout << "\t0 <= #Rows < " <<  (mGeomDb->numberOfRowsInAColumn())
	     << " (" << nearestInteger(raw.row()) << ") " << raw.row() << endl;
	return -9999;
    }
}

int StRichCoordinateTransform::checkQuadrantBounds(const StRichQuadrantCoordinate& quad) const
{
    if( (quad.quadrant() < 1) || (quad.quadrant() > 4) ) {
	cout << "StRichCoordinateTransform::checkQuadBounds()\n";
	cout << "\tWARNING:\n";
	cout << "\t1 <= Quadrant # <= 4  (" << quad.quadrant() << ")" << endl;
	return -9999;
    }

    bool warningMessage = false;
    switch(quad.quadrant()) {
    case 1:
	if( (quad.position().x() < ( mXQBound-mXGap/2.))  ||
	    (quad.position().x() >  -mXQBound)  ||
	    (quad.position().y() < ( mYQBound-mYGap/2.))  ||
	    (quad.position().y() >  -mYQBound) )
	    warningMessage = true;
	break;
    case 2:
	if( (quad.position().x() <   mXQBound )  ||
	    (quad.position().x() > (-mXQBound+mXGap/2.))  ||
	    (quad.position().y() < ( mYQBound-mYGap/2.))  ||
	    (quad.position().y() > -mYQBound) )
	    warningMessage = true;
	break;
    case 3:
	if( (quad.position().x() <   mXQBound )  ||
	    (quad.position().x() > (-mXQBound+mXGap/2.))  ||
	    (quad.position().y() <   mYQBound )  ||
	    (quad.position().y() > (-mYQBound+mYGap/2.)) )
	    warningMessage = true;
	break;
    case 4:
	if( (quad.position().x() < ( mXQBound-mXGap/2.))  ||
	    (quad.position().x() >  -mXQBound )  ||
	    (quad.position().y() <   mYQBound )  ||
	    (quad.position().y() > (-mYQBound+mYGap/2.)) )
	    warningMessage = true;
	break;
    default:
	cout << "StRichCoordinateTransform::checkQuadrantBounds()\n";
	cout << "\tWARNING:\n";
	cout << "\tUnknown quadrant: (" << quad.quadrant() << ")" << endl;
	break;
    }
    
    if(warningMessage) {
	cout << "StRichCoordinateTransform::checkQuadrantBounds()\n";
	cout << "\tWARNING:\n";
	cout << '\t' << mXQBound << "  <= xq <= " <<  -mXQBound
	     << " (" << quad.position().x() << ")\n";
	cout << '\t' << mYQBound << "  <= yq <= " <<  -mYQBound
	     << " (" << quad.position().y() << ")" << endl;
	cout << "\tzq --> Unrestricted " << endl;
	return -9999;
    }
    else {
	return 0;
    }
}

int StRichCoordinateTransform::checkLocalBounds(const StRichLocalCoordinate& local) const
{
    if( (local.position().x() <= mXLBound)  ||
	(local.position().x() >= -mXLBound) ||
	(local.position().y() <= mYLBound)  ||
	(local.position().y() >= -mYLBound) ) {
	
	cout << "StRichCoordinateTransform::checkLocalBounds()\n";
	cout << "\tWARNING:\n";
	cout << '\t' << mXLBound << "  <= xq <= " <<  -mXLBound
	     << " (" << local.position().x() << ")" << endl;
	cout << '\t' << mYLBound << "  <= yq <= " <<  -mYLBound
	     << " (" << local.position().y() << ")" << endl;
	cout << "\tzq --> Unrestricted " << endl;
	return -9999;
    }
    else {
	return 0;
    }
	    
	
}
