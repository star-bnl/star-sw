/***********************************************************************
 * $Id: StRichCoordinateTransform.cxx,v 1.2 2000/02/08 23:49:18 lasiuk Exp $
 *
 * Author: brian made this on Jan 27, 2000
 *
 ***********************************************************************
 * Description:
 *
 ***********************************************************************
 *
 * $Log: StRichCoordinateTransform.cxx,v $
 * Revision 1.2  2000/02/08 23:49:18  lasiuk
 * rm fpad from routine which is not returned
 *
 *
 * Revision 1.3  2000/02/24 18:44:14  lasiuk
 * coerce quadrant bounds limits
 *
 * Revision 1.2  2000/02/08 23:49:18  lasiuk
 * rm fpad from routine which is not returned
 *
 * Revision 1.1  2000/02/08 16:34:03  lasiuk

    return mInstance;
}


StRichCoordinateTransform::StRichCoordinateTransform()
{/* nopt */}

StRichCoordinateTransform::StRichCoordinateTransform(StRichGeometryDbInterface* geo)
    : mGeomDb(geo)
{

    mRowPitch = mGeomDb->rowPitch();
    mXQBound = -(mNumberOfRowsInAQuadrantColumn+.5)*mRowPitch/2;
    mZQBound = -(mNumberOfPadsInAQuadrantRow+.5)*mPadPitch/2.;
//      mXQBound = -(mNumberOfRowsInAQuadrantColumn+1.)*mRowPitch/2;
//      mZQBound = -(mNumberOfPadsInAQuadrantRow+1.)*mPadPitch/2.;
    mXLBound = -(mGeomDb->quadrantGapInX()/2. + (mNumberOfRowsInAQuadrantColumn+.5)*mRowPitch);
    mZLBound = -(mGeomDb->quadrantGapInZ()/2. + (mNumberOfPadsInAQuadrantRow+.5)*mPadPitch);

    // For Bounds check of local
    mXLBound = -(mGeomDb->quadrantGapInX()/2. + (mNumberOfPadsInAQuadrantRow+.5)*mPadPitch);
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
    b.position().setX(row2QuadX(a.row(), b.quadrant()));
    b.position().setY(0);
    b.position().setZ(pad2QuadZ(a.pad(), b.quadrant()));

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
    b.setRow(quadX2Row(a.position().x(), a.quadrant()));
    b.setPad(quadZ2Pad(a.position().z(), a.quadrant()));
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
    b.position().setY(a.position().y());
    b.position().setZ(a.position().z()+mGeomDb->quadrantZOrigin(a.quadrant()));

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
    b.position().setY( a.position().y());
    b.position().setZ( a.position().z()-mGeomDb->quadrantZOrigin(b.quadrant()) );
    b.setQuadrant( whichQuadrant(a) );
    b.position().setX( a.position().x()-mGeomDb->quadrantXOrigin(b.quadrant()) );
    b.position().setY( a.position().y()-mGeomDb->quadrantYOrigin(b.quadrant()) );
    b.position().setZ( a.position().z() );

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int quadStatus = checkQuadrantBounds(b);
#endif
}
    
    //  ( x_g )     ( cos Þ   -sin Þ    0  ) ( x_l )     (  dx )
    //  ( y_g )  =  ( sin Þ    cos Þ    0  ) ( y_l )  +  ( -dy )
    //  ( z_g )     (  0        0       1  ) ( z_l )     (  dz )
{   
    //  ( x_g )     ( cos Þ   -sin Þ    0  ) ( y_l )     (  dx_g )
    //  ( y_g )  =  ( sin Þ    cos Þ    0  ) ( z_l )  +  ( -dy_g )
    // dx = radialDistance*sin(Þ)
    // dy = radialDistance*cos(Þ)
    // where:
    // Þ = inclinationAngle (shift-alt p)
    // dx_g = radialDistance*sin(Þ)
    // dy_g = radialDistance*cos(Þ)
    //
    // Allow additional parameters from survey

#ifdef RICH_COORDINATE_BOUNDS_CHECK
    int localStatus = checkLocalBounds(a);
#endif
    double cosB   = cos(mInclinationAngle);
    double sinB   = sin(mInclinationAngle);

    b.position().setX( cosB*xLocal - sinB*yLocal + mRadialDistanceToRich*sinB );
    b.position().setY( sinB*xLocal + cosB*yLocal - mRadialDistanceToRich*cosB );
    b.position().setZ(zLocal);
    
    b.position().setX( mCosB*yLocal - mSinB*zLocal + mRadialDistanceToRich*mSinB );
    b.position().setY( mSinB*yLocal + mCosB*zLocal - mRadialDistanceToRich*mCosB );
    b.position().setZ(xLocal);
}

void
    // (  cos Þ    sin Þ    0  ) ( x_g - dx )     ( 1   0    0  ) ( x_l )
    // ( -sin Þ    cos Þ    0  ) ( y_g + dy )  =  ( 0   1    0  ) ( y_l )
    // (   0        0       1  ) ( z_g - dz )     ( 0   0    1  ) ( z_l )

    // (  cos Þ    sin Þ    0  ) ( x_g - dx_g )     ( 1   0    0  ) ( y_l )
    // ( -sin Þ    cos Þ    0  ) ( y_g + dy_g )  =  ( 0   1    0  ) ( z_l )
    // (   0        0       1  ) ( z_g - dz_g )     ( 0   0    1  ) ( x_l )
    //
    // where:
    // Þ = inclinationAngle (shift-alt p)
    // dx = radialDistance*sin(Þ)
    double cosB   = cos(mInclinationAngle);
    double sinB   = sin(mInclinationAngle);

    double xGlobalShift = a.position().x() - mRadialDistanceToRich*sinB;
    double yGlobalShift = a.position().y() + mRadialDistanceToRich*cosB;
    // Allow additional parameters from survey

    b.position().setX( cosB*(xGlobalShift) + sinB*(yGlobalShift) );
    b.position().setY(-sinB*(xGlobalShift) + cosB*(yGlobalShift) );
    b.position().setZ(zGlobalShift);
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

	// in quad 1 or 2
	if(loc.position().z() < 0 ) {
	    return 2;
    if(loc.position().x() > 0 ) {
	// in quad 1 or 4
	    return 1;
	    return 1;
	}
    else { // must be quad 3 or 4
	if(loc.position().z() < 0 ) {
	    return 3;
    }
    else { // must be quad 2 or 3
	    return 4;
	    return 2;
	}
	else {
	    return 3;
	}
StRichCoordinateTransform::row2QuadX(int row, int quad) const
}
    int pseudoRow = row;
double
StRichCoordinateTransform::row2QuadY(double row, int quad) const
{
    double pseudoRow = row;

    if(quad == 3 || quad == 4) {
	pseudoRow = (row - mNumberOfRowsInAQuadrantColumn);
    }

StRichCoordinateTransform::pad2QuadZ(int pad, int quad) const
}
    
    int pseudoPad = pad;
StRichCoordinateTransform::pad2QuadX(double pad, int quad) const
{
	
    double pseudoPad = pad;
    if(quad == 1 || quad == 4) {
	pseudoPad = (pad - mNumberOfPadsInAQuadrantRow);
    }
int
StRichCoordinateTransform::quadX2Row(double qx, int quad) const
}

    int rowOffset = 0;
StRichCoordinateTransform::quadY2Row(double qx, int quad) const
{
    }
    else {
    return (nearestInteger( rowOffset + ((-qx/mRowPitch - .5) + mNumberOfRowsInAQuadrantColumn/2.)) );
	    qx = mYQBound;
    }
int
StRichCoordinateTransform::quadZ2Pad(double qz, int quad) const
}
    int padOffset = 0;
double
StRichCoordinateTransform::quadX2Pad(double qz, int quad) const
    }
    
    return (nearestInteger(padOffset + ((qz/mPadPitch - .5) + mNumberOfPadsInAQuadrantRow/2.)) );
	if(qz>=-mXQBound)
	    qz = -mXQBound;
int
StRichCoordinateTransform::nearestInteger(double x) const
{
    return ( static_cast<int>(x+.5) );
}
// StRichCoordinateTransform::nearestInteger(double x) const
// {
//     return ( static_cast<int>(x+.5) );
// }

//
/**************************************************************************************/
// These routines check the bounds of the values when the MACRO
    if( ((raw.pad() < mGeomDb->numberOfPadsInARow())    && (raw.pad() >= 0)) &&
	((raw.row() < mGeomDb->numberOfRowsInAColumn()) && (raw.row() >= 0))) {
    if( ((nearestInteger(raw.pad()) < mGeomDb->numberOfPadsInARow())    &&
	 (nearestInteger(raw.pad()) >= 0) )                             &&
	((nearestInteger(raw.row()) < mGeomDb->numberOfRowsInAColumn()) &&
	cerr << "WARNING:StRichCoordinateTransform::checkRawBounds()"    << endl;
	cerr << " 0 <= #Pads < " <<  (mGeomDb->numberOfPadsInARow())
	     << " (" << raw.pad() << ")" << endl;
	cerr << " 0 <= #Rows < " <<  (mGeomDb->numberOfRowsInAColumn())
	     << " (" << raw.row() << ")" << endl;
	cout << "\t0 <= #Pads < " <<  (mGeomDb->numberOfPadsInARow())
	     << " (" << nearestInteger(raw.pad()) << ") " << raw.pad() << endl;
	cout << "\t0 <= #Rows < " <<  (mGeomDb->numberOfRowsInAColumn())
	     << " (" << nearestInteger(raw.row()) << ") " << raw.row() << endl;
	return -9999;
    }
}
	cerr << "WARNING:StRichCoordinateTransform::checkQuadBounds()"    << endl;
	cerr << " 1 <= Quadrant # <= 4  (" << quad.quadrant() << ")" << endl;
    if( (quad.quadrant() < 1) || (quad.quadrant() > 4) ) {
	cout << "StRichCoordinateTransform::checkQuadBounds()\n";
	
    if( (quad.position().x() <= mXQBound)  ||
	(quad.position().x() >= -mXQBound) ||
	(quad.position().z() <= mZQBound)  ||
	(quad.position().z() >= -mZQBound) ) {
	
	cerr << "WARNING:StRichCoordinateTransform::checkQuadrantBounds()"    << endl;
	cerr << mXQBound << "  <= xq <= " <<  -mXQBound
	     << " (" << quad.position().x() << ")" << endl;
	cerr << "  yq --> Unrestricted " << endl;
	cerr << mZQBound << "  <= zq <= " <<  -mZQBound
	     << " (" << quad.position().z() << ")" << endl;
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

	(local.position().z() <= mZLBound)  ||
	(local.position().z() >= -mZLBound) ) {
    if( (local.position().x() <= mXLBound)  ||
	cerr << "WARNING:StRichCoordinateTransform::checkLocalBounds()"    << endl;
	cerr << mXLBound << "  <= xq <= " <<  -mXLBound
	
	cerr << "  yq --> Unrestricted " << endl;
	cerr << mZLBound << "  <= zq <= " <<  -mZLBound
	     << " (" << local.position().z() << ")" << endl;
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
