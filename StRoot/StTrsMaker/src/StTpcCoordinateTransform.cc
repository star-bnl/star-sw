/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.cc,v 1.2 1999/01/15 11:03:59 lasiuk Exp $
 *
 * Author: brian Feb 6, 1998
 *
 ***********************************************************************
 * Description:
 *
 * Geometrical transformation Routines for:
 * Raw Pad Coordinate  <-->  Local Coordinate
 *   Local Coordinate  <-->  Global Coordinate
 *
 * These Routines deal positions ONLY!
 *
 ***********************************************************************
 *
 * $Log: StTpcCoordinateTransform.cc,v $
 * Revision 1.2  1999/01/15 11:03:59  lasiuk
 * sector 12/24 compatibility
 *
 * Revision 1.6  1999/02/16 18:15:41  fisyak
 * Check in the latest updates to fix them
 *
 * Revision 1.5  1999/02/12 01:26:36  lasiuk
 * Limit debug output
 *
 * Revision 1.4  1999/02/10 04:23:24  lasiuk
 * HP namespace
 *
 * Revision 1.3  1999/01/28 02:51:27  lasiuk
 * add ()localSector --> Raw
 * add ()localSector --> Local
 *
 * Revision 1.2  1999/01/15 11:03:59  lasiuk
 * sector 12/24 compatibility
 *
 * Revision 1.1  1998/11/10 17:12:20  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/01 16:20:36  lasiuk
 * remove 'St' from variable declarations
 *
 * Revision 1.4  1998/10/22 00:24:19  lasiuk
 * Oct 22
 *
 * Revision 1.2  1998/05/25 17:05:25  lasiuk
StTpcCoordinateTransform::StTpcCoordinateTransform(StTpcGeometry* geomdb, StTpcSlowControl* scdb)
{
 * use databases instead of filenames
 *
StTpcCoordinateTransform::StTpcCoordinateTransform(StTpcGeometry* geomdb, StTpcSlowControl* scdb)
 *
#include "StTpcCoordinateTransform.hh"
#include "StMatrix.hh"

						   StTpcSlowControl* scdb,
						   StTpcElectronics* eldb)
    : mRotation(2,2,1), mRotate(2,1,0), mResult(2,1,0) {
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a, StGlobalCoordinate& b) const 
    mTPCdb = geomdb;
    mSCdb  = scdb;
    mElectronicsDb = eldb;
    mTimeBinWidth = 1./mElectronicsDb->samplingFrequency();
    //system("/opt/audio/bin/send_sound -server xstar3 /home/star/ullrich/tmp/sounds/ferrari.au &");
    //system("/opt/audio/bin/send_sound -server xstar3 /home/star/ullrich/tmp/sounds/james-bond.au &");
    //system("/home/star/lasiuk/bin/xmelt -display xstar1:0 &");
void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a, StTpcPadCoordinate& b) const

StTpcCoordinateTransform::~StTpcCoordinateTransform() { /* nopt */ }

//      Raw Data          <-->  Global Coordinate
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a, StGlobalCoordinate& b)
{
    StTpcLocalCoordinate tmp;
    
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a, StTpcLocalCoordinate& b) const 
    if(a.sector() > 12)
}
void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a, StTpcPadCoordinate& b)
{
    StTpcLocalCoordinate tmp;

    this->operator()(a,tmp);
    this->operator()(tmp,b);
}

//      Raw Data          <-->  Tpc Local Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate&  a, StTpcPadCoordinate& b) const 
{
    // Covert to xy and rotate into local sector frame (12)
    StThreeVector<double> tmp = xyFromRaw(a);
    if(a.sector() > 12) {
	tmp.setZ(-(zFromTB(a.timeBucket())));
    }
    else
	tmp.setZ((zFromTB(a.timeBucket())));

    b = StTpcLocalCoordinate(tmp);
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate&  a, StTpcPadCoordinate& b)
    //PR(tb);
    int sector = sectorFromCoordinate(a);
    if(a.pos().z() < 0)
    // rotate xy to the appropriate sector!
    int decodedVolumeId = a.volumeId();
//      Local Coordinate  <-->  Tpc Local Coordinate
    int isdet = (decodedVolumeId/100000);
    decodedVolumeId -= isdet*100000;
    int sector = decodedVolumeId/100;
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StGlobalCoordinate& b) const 
    //PR(row);

    double yOffsetInSector12 = mTPCdb->radialDistanceAtRow(row);
    StThreeVector<double> sector12Position(a.pos().x(),
					   a.pos().y() + yOffsetInSector12,
					   a.pos().z() + mTPCdb->frischGrid());
    StThreeVector<double> tmp = rotateFromLocal(sector12Position,sector);
void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a, StTpcLocalCoordinate& b) const 
    b = StTpcLocalCoordinate(tmp);
}

//      Local Coordinate  <--> Global Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StGlobalCoordinate& b) 
{
    // Requires survey DB i/o!
StThreeVector<double> StTpcCoordinateTransform::sector12Coordinate(StThreeVector<double>& v, int *sector) const

    b = StGlobalCoordinate(a.pos());
}

void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a, StTpcLocalCoordinate& b)
{
    // Requires survey DB i/o!
StTpcCoordinateTransform::padCentroid(StTpcLocalCoordinate& local, int *pad, int *row) const

    b = StTpcLocalCoordinate(a.pos());   
}

StThreeVector<double> StTpcCoordinateTransform::sector12Coordinate(StThreeVector<double>& v, int *sector)
{
    *sector = sectorFromCoordinate(v);
//     sec12 = rotateToLocal(v,sector);
    return  rotateToLocal(v,*sector);
}

StThreeVector<double>
StTpcCoordinateTransform::padCentroid(StTpcLocalCoordinate& local, int *pad, int *row)
{
    StTpcLocalCoordinate centerOfPad;
    int nRow = rowFromLocal(local.pos());
    StTpcPadCoordinate tmp(12,                      //sector
			   nRow,     //row
			   padFromLocal(local.pos(),nRow), // pad
			   0);
    
    this->operator()(tmp,centerOfPad);
    *row = nRow;
    return centerOfPad.pos();
}
/***********************************************************************/
/*                       TRANSFORMATION ROUTINES                       */

int StTpcCoordinateTransform::sectorFromCoordinate(const StTpcLocalCoordinate& a) const
{
    const double anglePerSector = M_PI/6;  // 30 degrees should be from db

    double angle = atan2(a.pos().y(),a.pos().x());
    if(angle<0) angle+= 2*M_PI;

     if(angle>=0 && angle<= M_PI/2)
	 angle = M_PI/2 - angle;
     else
	 angle = 5*M_PI/2 - angle;

     double sector = (angle + anglePerSector/2)/anglePerSector;

     int sectorNumber = (sector<1) ? 12 : (int)sector;
     return(sectorNumber);
}

int StTpcCoordinateTransform::sectorFromCoordinate(const StThreeVector<double>& a) const
{
    const double anglePerSector = M_PI/6;  // 30 degrees should be from db

    double angle = atan2(a.y(),a.x());
    if(angle<0) angle+= 2*M_PI;

     if(angle>=0 && angle<= M_PI/2)
	 angle = M_PI/2 - angle;
     else
	 angle = 5*M_PI/2 - angle;

     double sector = (angle + anglePerSector/2)/anglePerSector;

     int sectorNumber = (sector<1) ? 12 : (int)sector;
     return(sectorNumber);
}

// FOR SECTOR 12 ONLY!!!! (Local coordinate);
int StTpcCoordinateTransform::rowFromLocal(const StThreeVector<double>& b) const
{
    double referencePosition;
    double rowPitch;
    int    offset;
    double innerSectorBoundary =
	mTPCdb->outerSectorEdge() - mTPCdb->ioSectorSpacing();

    if(b.y() > innerSectorBoundary) {    // in the outer sector
	referencePosition = mTPCdb->radialDistanceAtRow(14);
	rowPitch          = mTPCdb->outerSectorRowPitch();
	offset            = 14;
    }
    else if(b.y() > mTPCdb->radialDistanceAtRow(8)) {
	referencePosition = mTPCdb->radialDistanceAtRow(8);
	rowPitch          = mTPCdb->innerSectorRowPitch2();
	offset            = 8;
    }
    else {
	referencePosition = mTPCdb->radialDistanceAtRow(1);
	rowPitch          = mTPCdb->innerSectorRowPitch1();
	offset            = 1;	
    }

//     PR(b.y());
//     PR(referencePosition);
//     PR(rowPitch);
//     PR(offset);
    int probableRow =
	static_cast<int>( (b.y() - (referencePosition-rowPitch/2))/rowPitch )+offset;

    if(b.y() > innerSectorBoundary && probableRow<14)
	probableRow=14;

    if (probableRow<1)
	probableRow = 1;
    if (probableRow>45)
	probableRow=45;
    
//     PR(probableRow);

    return (probableRow);
}


int StTpcCoordinateTransform::padFromLocal(const StThreeVector<double>& b, const int row) const
{
    

    double thePitch = (row<=13) ?
    if(probablePad<1) probablePad = 1;
    
    idb << "Number of Pads (shift): " << numberOfPads << endl;

    probablePad += numberOfPads;

    // CAUTION: pad cannot be <1
    if(probablePad<1) {
StThreeVector<double> StTpcCoordinateTransform::xyFromRaw(const StTpcPadCoordinate& a) const
// 	cerr << "Pad is calculated to be '" << probablePad << "'\n";
// 	cerr << "Assigning Pad='1'"<< endl;
	probablePad=1;
    }

    return (probablePad);
}


//
    double localX = xFromPad(a.row(),a.pad());

    
    double localX = (a.sector()>12) ?
	-1.*xFromPad(a.row(),a.pad()) :
	xFromPad(a.row(),a.pad());

    // rotate properly
    StThreeVector<double> newxy =
	rotateFromLocal(StThreeVector<double>(localX,localY,0), a.sector());

    return(newxy);
}

//Local Transformation...
double StTpcCoordinateTransform::yFromRow(const int row)  const
{
    // Returns y coordinate in sector 12
    return (mTPCdb->radialDistanceAtRow(row));
}


double StTpcCoordinateTransform::xFromPad(const int row, const int pad) const
{
    // x coordinate in sector 12
    double pitch = (row<14) ?
	mTPCdb->innerSectorPadPitch() :
	mTPCdb->outerSectorPadPitch();

    int pads2move = pad - (mTPCdb->numberOfPadsAtRow(row))/2;
    //    double tb = (mTPCdb->frischGrid - z)/mTPCdb->vD;
    double z = mTPCdb->frischGrid() - (mSCdb->driftVelocity()*tb);
    return(nearestInteger(tb));
#ifndef ST_NO_NAMESPACES
    // I do not like this.  This should be passed via the electronics data base!
    // We will have to see what is decided about this code!
    {
    return(nearestInteger(tb/(100.*nanosecond)));
    //PR(tb/(100.*nanosecond));
	//PR(mTimeBinWidth/nanosecond);
    //PR(mTPCdb->frischGrid());
#ifndef ST_NO_NAMESPACES    
    }
#endif
				     const int sector) const
    //PR(mSCdb->driftVelocity());
    double tb = (mTPCdb->frischGrid() - mElectronicsDb->tZero()*mSCdb->driftVelocity() - z)/mSCdb->driftVelocity();

    return(nearestInteger(tb/(mTimeBinWidth)));
}

//

    const int m = 2;  
    const int n = 2;
    StMatrix<double> m1(m,n,0);

    // vector to be rotated
    StMatrix<double> v1(2,1,0);
    v1(1,1) = a.x();
    v1(2,1) = a.y();  // z co-ordinate is immaterial

    double beta = sector*M_PI/6;   //(30 degrees)
  
    m1(1,1) = cos(beta);
    m1(1,2) = -sin(beta);

    m1(2,1) = -1*m1(1,2); // saves calculation sin(beta);
    m1(2,2) = m1(1,1);    // saves calculation cos(beta);

    //PR(m1);
    StMatrix<double> newMatrix = m1*v1;
    //PR(newMatrix);
    return(StThreeVector<double>(newMatrix(1,1),newMatrix(2,1),a.z()));
    // Now modify the data members instead:
    mRotation(1,1) =  cos(beta);
    mRotation(1,2) = -sin(beta);
						     const int sector) const
    mRotation(2,2) =    mRotation(1,1);   // saves calculation cos(beta);

    mRotate(1,1) = a.x();
    mRotate(2,1) = a.y();  // z co-ordinate is immaterial

    mResult = mRotation*mRotate;
    
    const int m = 2;  
    const int n = 2;
    StMatrix<double> m1(m,n,0);
//     PR(mResult);
    // vector to be rotated
    StMatrix<double> v1(2,1,0);

    v1(1,1) = a.x();
    v1(2,1) = a.y();  // z co-ordinate is immaterial

    double beta = -sector*M_PI/6;   //(30 degrees)  NEGATIVE ANGLE!!!!!!!!
    idb << "Rotation angle is " << beta << endl;
    m1(1,1) = cos(beta);
    m1(1,2) = -sin(beta);

    m1(2,1) = -1*m1(1,2); // saves calculation sin(beta);
    m1(2,2) = m1(1,1);   // saves calculation cos(beta);

    StMatrix<double> newMatrix = m1*v1;
	
    return(StThreeVector<double>(newMatrix(1,1),newMatrix(2,1),a.z()));
    // New code:
    //
    mRotation(1,1) =  cos(beta);
    mRotation(1,2) = -sin(beta);
    mRotation(2,1) = -1.*mRotation(1,2);   // saves calculation sin(beta);
    mRotation(2,2) =     mRotation(1,1);   // saves calculation cos(beta);

    mRotate(1,1) = a.x();
    mRotate(2,1) = a.y();  // z co-ordinate is immaterial

    mResult = mRotation*mRotate;
//     PR(mResult);
    return(StThreeVector<double>(mResult(1,1),mResult(2,1),a.z()));
}


/****************************************************************/
/*                         UTILITIES                            */
double StTpcCoordinateTransform::rad2deg(double a) const
{
    return(57.2957795*a);
}

int StTpcCoordinateTransform::nearestInteger(double a) const
{
    return static_cast<int>(a);
}
