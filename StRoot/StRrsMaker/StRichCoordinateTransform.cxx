/***********************************************************************
 * $Id: StRichCoordinateTransform.cxx,v 1.4 2000/03/12 22:20:24 lasiuk Exp $
 *
 * Author: brian made this on Jan 27, 2000
 *
 ***********************************************************************
 * Description:
 *
 ***********************************************************************
 *
 * $Log: StRichCoordinateTransform.cxx,v $
 * Revision 1.4  2000/03/12 22:20:24  lasiuk
 * make into a singleton class
 * incliniation angle stored as data members
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

    // Survey
    mInclinationAngle     = mGeomDb->inclinationAngle();
    mCosB                 = cos(mInclinationAngle);
    mSinB                 = sin(mInclinationAngle);
    mRadialDistanceToRich = mGeomDb->radialDistanceToRich();
}

StRichCoordinateTransform::~StRichCoordinateTransform()
{/*nopt*/}

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
    //  ( x_g )     ( cos Þ   -sin Þ    0  ) ( y_l )     (  dx_g )
    //  ( y_g )  =  ( sin Þ    cos Þ    0  ) ( z_l )  +  ( -dy_g )
    //  ( z_g )     (  0        0       1  ) ( x_l )     (  dz_g )
    //
    // where:
    // Þ = inclinationAngle (shift-alt p)
    // dx_g = radialDistance*sin(Þ)
    // dy_g = radialDistance*cos(Þ)
    //
    // Allow additional parameters from survey

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(a);
#endif

    double xLocal = a.position().x();
    double yLocal = a.position().y();
    double zLocal = a.position().z();
    
    b.position().setX( mCosB*yLocal - mSinB*zLocal + mRadialDistanceToRich*mSinB );
    b.position().setY( mSinB*yLocal + mCosB*zLocal - mRadialDistanceToRich*mCosB );
    b.position().setZ(xLocal);
}

void
StRichCoordinateTransform::operator()(const StGlobalCoordinate& a, StRichLocalCoordinate& b)
{
    // Inverse of above transformation!

    // (  cos Þ    sin Þ    0  ) ( x_g - dx_g )     ( 1   0    0  ) ( y_l )
    // ( -sin Þ    cos Þ    0  ) ( y_g + dy_g )  =  ( 0   1    0  ) ( z_l )
    // (   0        0       1  ) ( z_g - dz_g )     ( 0   0    1  ) ( x_l )
    //
    // where:
    // Þ = inclinationAngle (shift-alt p)
    // dx = radialDistance*sin(Þ)
    // dy = radialDistance*cos(Þ)
    //
    // Allow additional parameters from survey

    double xGlobalShift = a.position().x() - mRadialDistanceToRich*mSinB;
    double yGlobalShift = a.position().y() + mRadialDistanceToRich*mCosB;
    double zGlobalShift = a.position().z();
    
    b.position().setX(zGlobalShift);
    b.position().setY( mCosB*(xGlobalShift) + mSinB*(yGlobalShift) );
    b.position().setZ(-mSinB*(xGlobalShift) + mCosB*(yGlobalShift) );
//     b.position().setX( cosB*(xGlobalShift) + sinB*(yGlobalShift) );
//     b.position().setY(-sinB*(xGlobalShift) + cosB*(yGlobalShift) );
//     b.position().setZ(zGlobalShift);

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
