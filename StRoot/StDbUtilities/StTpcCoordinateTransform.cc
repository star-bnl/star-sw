/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.cc,v 1.1 1999/11/19 19:01:08 calderon Exp $
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
 * Revision 1.1  1999/11/19 19:01:08  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.14  1999/10/25 18:38:49  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.13  1999/10/04 15:21:58  long
 * new coordinate system in the trs
 *
 * Revision 1.13  1999/10/01 17:15:00  Hui Long
 * using  new coordinate system proposed by Brian and Thomas
 * See their proposal for the new definition of the coordinate systems 
 * Revision 1.12  1999/07/19 21:40:13  lasiuk
 * local->raw transform redefine origin for shift offset calculation
 *
 * Revision 1.11  1999/03/15 13:48:20  lasiuk
 * xyFromRaw is changed to take into account the inversion.
 * The local coordinate system should be rechecked to make
 * sure it is defined as the STAR Coordinate system!
 *
 * Revision 1.10  1999/03/02 17:52:10  lasiuk
 * rotation for sectors>12
 *
 * Revision 1.9  1999/02/24 19:31:25  lasiuk
 * allow for tZero offset
 * positive pushes time bins into the chamber
 *
 * Revision 1.8  1999/02/18 21:17:27  lasiuk
 * instantiate with electronics db
 *
 * Revision 1.7  1999/02/16 23:28:59  lasiuk
 * matrix(3) is a data member to avoid constructor calls
 * protection against pad<1
 * const removed from several functions (because of matrix)
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
 * Revision 1.3  1998/06/04 23:24:09  lasiuk
 * add sector12 coordinate transform as a public member
 *
 * Revision 1.2  1998/05/25 17:05:25  lasiuk
 * use databases instead of filenames
 *
 * Revision 1.1  1998/05/21 21:27:57  lasiuk
 * Initial revision
 *
 *
 ***********************************************************************/
#include "StTpcCoordinateTransform.hh"
#include "StMatrix.hh"
#include <unistd.h>
// StTpcCoordinateTransform::StTpcCoordinateTransform(StTpcGeometry* geomdb,
// 						   StTpcSlowControl* scdb,
// 						   StTpcElectronics* eldb)

StTpcCoordinateTransform::StTpcCoordinateTransform(StTpcDb* globalDbPointer)
: mRotation(2,2,1), mRotate(2,1,0), mResult(2,1,0) {
  
//     mTPCdb = geomdb;
//     mSCdb  = scdb;
//     mElectronicsDb = eldb;
     gTpcDbPtr    = globalDbPointer;
    mTimeBinWidth = 1./gTpcDbPtr->Electronics()->samplingFrequency();
    //
    // For this version I'll put the inner/outer sector z offsets by
    // hand, since StTpcDb doesn't have them.  I'll take them out when
    // it does.
    mInnerSectorzOffset = 3.5 * millimeter;
    mOuterSectorzOffset = 0.;
}

StTpcCoordinateTransform::~StTpcCoordinateTransform() { /* nopt */ }

//      Raw Data          <-->  Global Coordinate
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a, StGlobalCoordinate& b)
{
    StTpcLocalCoordinate tmp2;
    StTpcLocalSectorCoordinate tmp1;
    this->operator()(a,tmp1);
    this->operator()(tmp1,tmp2); 
    this->operator()(tmp2,b);
}

void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a, StTpcPadCoordinate& b)
{

    StTpcLocalCoordinate tmp1;
    StTpcLocalSectorCoordinate tmp2;
    this->operator()(a,tmp1);
    this->operator()(tmp1,tmp2); 
    this->operator()(tmp2,b);
}
//      Raw Data          <-->  TPC Local  Coordinate
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a, StTpcLocalCoordinate& b)
{
    
    StTpcLocalSectorCoordinate tmp;
    this->operator()(a,tmp);
  
    this->operator()(tmp,b);
}

void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StTpcPadCoordinate& b)
{

   
    StTpcLocalSectorCoordinate tmp;
    this->operator()(a,tmp);
    
    this->operator()(tmp,b);
}



//      Local Sector Coordnate    <->  Tpc Raw Pad Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate& a, StTpcPadCoordinate& b)
{
  
    int sector = a.fromSector();
    int row    = rowFromLocal(a.position());
    
    int probablePad =padFromLocal(a.position(),row);
    double zoffset;
    zoffset=(row>13) ?
// 	gTpcDbPtr->PadPlaneGeometry()->outerSectorzOffSet()
//       :gTpcDbPtr->PadPlaneGeometry()->innerSectorzOffSet() ;
	mOuterSectorzOffset
	:mInnerSectorzOffset;
    int tb = tBFromZ(a.position().z()+zoffset);
    b = StTpcPadCoordinate(sector, row, probablePad, tb);
}
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a,  StTpcLocalSectorCoordinate& b)
{
    StThreeVectorF  tmp=xyFromRaw(a);
    double zoffset= (a.row()>13) ?
// 	gTpcDbPtr->PadPlaneGeometry()->outerSectorzOffSet()
// 	:gTpcDbPtr->PadPlaneGeometry()->innerSectorzOffSet() ;
	mOuterSectorzOffset
	:mInnerSectorzOffset;

    tmp.setZ(zFromTB(a.timeBucket())-zoffset);
    b = StTpcLocalSectorCoordinate(tmp,a.sector());
}
//  Tpc Local Sector <--> Global
 
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate& a, StGlobalCoordinate& b)
{
     StTpcLocalCoordinate tmp;
     this->operator()(a,tmp);
     this->operator()(tmp,b); 
    
}
void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a,  StTpcLocalSectorCoordinate& b)
{
    StTpcLocalCoordinate tmp;
    this->operator()(a,tmp);
    this->operator()(tmp,b); 
}
//      Local Sector Coordnate    -->  Tpc Local Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate& a, StTpcLocalCoordinate& b)
{
   
      int sector = a.fromSector();
    


    StThreeVectorF sector12Position(a.position().x(),
					   a.position().y() ,
					   a.position().z());
    StThreeVectorF tmp = rotateToLocal(sector12Position,sector);

    b = StTpcLocalCoordinate(tmp);
}
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StTpcLocalSectorCoordinate& b)
{   int sector= sectorFromCoordinate(a);
    StThreeVectorF tmp=rotateFromLocal(a.position(),sector);
    
    b = StTpcLocalSectorCoordinate(tmp,sector);
}
//      Local Coordinate  <--> Global Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StGlobalCoordinate& b) 
{
    // Requires survey DB i/o!
    // Take as unity for now

    b = StGlobalCoordinate(a.position());
}

void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a, StTpcLocalCoordinate& b)
{
    // Requires survey DB i/o!
    // Take as unity for now

    b = StTpcLocalCoordinate(a.position());   
}

StThreeVectorF StTpcCoordinateTransform::sector12Coordinate(StThreeVectorF& v, int *sector)
{
    *sector = sectorFromCoordinate(v);
//     sec12 = rotateToLocal(v,sector);
    return  rotateFromLocal(v,*sector);
}

StThreeVectorF
StTpcCoordinateTransform::padCentroid(StTpcLocalSectorCoordinate& localSector, int *pad, int *row)
{
    StTpcLocalSectorCoordinate centerOfPad;
    int nRow = rowFromLocal(localSector.position());
    StTpcPadCoordinate tmp(12,                      //sector
			   nRow,     //row
			   padFromLocal(localSector.position(),nRow), // pad
			   localSector.fromSector());
    
    this->operator()(tmp,centerOfPad);
    *row = nRow;
    return centerOfPad.position();
}
/***********************************************************************/
/*                       TRANSFORMATION ROUTINES                       */

int StTpcCoordinateTransform::sectorFromCoordinate(const StTpcLocalCoordinate& a) const
{
   // 30 degrees should be from db

    double angle = atan2((a.position()).y(),(a.position()).x());
    if(angle<0) angle+= 2*M_PI;
    int sectorNumber= (int)( (angle+4*M_PI/3.)/(2*M_PI/3.));
    if((a.position()).z()>0){
               sectorNumber=15-sectorNumber;
               if(sectorNumber>12)sectorNumber-=12;
               }
    else
              {
               sectorNumber+=9;
               if(sectorNumber<12)sectorNumber+=12;
               }

    
     return(sectorNumber);
}
// sector from Tpc local coordinates
int StTpcCoordinateTransform::sectorFromCoordinate(const StThreeVectorF& a) const
{
    // 30 degrees should be from db

    double angle = atan2(a.y(),a.x());
    if(angle<0) angle+= 2*M_PI;
    int sectorNumber= (int)( (angle+4*M_PI/3.)/(2*M_PI/3.));
    if(a.z()>0){
               sectorNumber=15-sectorNumber;
               if(sectorNumber>12)sectorNumber-=12;
               }
    else
              {
               sectorNumber+=9;
               if(sectorNumber<12)sectorNumber+=12;
               }

     return(sectorNumber);
}

// FOR SECTOR 12 ONLY!!!! (Local coordinate);
int StTpcCoordinateTransform::rowFromLocal(const StThreeVectorF& b) const
{
    double referencePosition;
    double rowPitch;
    int    offset;
    double innerSectorBoundary =
      gTpcDbPtr->PadPlaneGeometry()->outerSectorEdge() -
      gTpcDbPtr->PadPlaneGeometry()->ioSectorSeparation();

    if(b.y() > innerSectorBoundary) {    // in the outer sector
	referencePosition = gTpcDbPtr->PadPlaneGeometry()->radialDistanceAtRow(14);
	rowPitch          = gTpcDbPtr->PadPlaneGeometry()->outerSectorRowPitch();
	offset            = 14;
    }
    else if(b.y() > gTpcDbPtr->PadPlaneGeometry()->radialDistanceAtRow(8)) {
	referencePosition = gTpcDbPtr->PadPlaneGeometry()->radialDistanceAtRow(8);
	rowPitch          = gTpcDbPtr->PadPlaneGeometry()->innerSectorRowPitch2();
	offset            = 8;
    }
    else {
	referencePosition = gTpcDbPtr->PadPlaneGeometry()->radialDistanceAtRow(1);
	rowPitch          = gTpcDbPtr->PadPlaneGeometry()->innerSectorRowPitch1();
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


int StTpcCoordinateTransform::padFromLocal(const StThreeVectorF& b, const int row) const
{
    int probablePad = gTpcDbPtr->PadPlaneGeometry()->numberOfPadsAtRow(row)/2;
    //cout << "Probable Pad: " << probablePad << endl;
    //cout << "Row " << row << " has " << gTpcDbPtr->PadPlaneGeometry()->numberOfPadsAtRow(row) << " pads." << endl;
   

    double thePitch = (row<=13) ?
	gTpcDbPtr->PadPlaneGeometry()->innerSectorPadPitch() :
	gTpcDbPtr->PadPlaneGeometry()->outerSectorPadPitch();

  
    //  double shift =  (b.x()+thePitch/2.)/thePitch;
    //shift = (b.x()<0) ? shift-0.5 : shift+0.5;
   
    //PR(shift);
    // shift in number of pads from centerline
    // int numberOfPads = static_cast<int>(shift); 
   double shift =  (b.x())/thePitch;//HL,8/31/99 
   int numberOfPads = static_cast<int>(shift); 
    numberOfPads  = (b.x()<0) ?  numberOfPads :numberOfPads+1 ;
    
    //cout << "Number of Pads (shift): " << numberOfPads << endl;

    probablePad += numberOfPads; 
   
    
    // CAUTION: pad cannot be <1
    if(probablePad<1) {
// 	cerr << "ERROR in pad From Local.\n";
// 	cerr << "Pad is calculated to be '" << probablePad << "'\n";
// 	cerr << "Assigning Pad='1'"<< endl;
	probablePad=1;
    }
    

    return (probablePad);
}


//
// Coordinate from Raw
//
StThreeVectorF StTpcCoordinateTransform::xyFromRaw(const StTpcPadCoordinate& a)
{
    double localY = yFromRow(a.row());
    // Just a test?
    //double localX = xFromPad(a.row(),a.pad());

    
    //   double localX = (a.sector()>12) ?
    //	-1.*xFromPad(a.row(),a.pad()) :
    //	xFromPad(a.row(),a.pad());
        double localX = 
	  xFromPad(a.row(),a.pad()) ;//HL,8/31/99
    

    // rotate properly
	// StThreeVectorF newxy =
	//	rotateFromLocal(StThreeVectorF(localX,localY,0), a.sector());

    return(StThreeVectorF(localX,localY,0));
}

//Local Transformation...
double StTpcCoordinateTransform::yFromRow(const int row)  const
{
    // Returns y coordinate in sector 12
    return (gTpcDbPtr->PadPlaneGeometry()->radialDistanceAtRow(row));
}


double StTpcCoordinateTransform::xFromPad(const int row, const int pad) const
{
    // x coordinate in sector 12
    double pitch = (row<14) ?
	gTpcDbPtr->PadPlaneGeometry()->innerSectorPadPitch() :
	gTpcDbPtr->PadPlaneGeometry()->outerSectorPadPitch();

    int pads2move = pad - (gTpcDbPtr->PadPlaneGeometry()->numberOfPadsAtRow(row))/2;
    double dist2move = pitch*(pads2move-.5);
 
    return(dist2move);
}

double StTpcCoordinateTransform::zFromTB(const int tb) const
{
    // StTpcDb doesn't have this in.  When it does, or when we can get
    // it from some of the available parameters, we'll put it in.
    double frischGrid = 2098.998*millimeter;
    
    double z = //gTpcDbPtr->PadPlaneGeometry()->frischGrid()-
	frischGrid-
	gTpcDbPtr->SlowControlSim()->driftVelocity()*(-gTpcDbPtr->Electronics()->tZero() + tb*mTimeBinWidth);  // z= tpc local sector  z,no inner outer offset yet.
   
    return(z);
}

int StTpcCoordinateTransform::tBFromZ(const double z) const
{
    // StTpcDb doesn't have this in.  When it does, or when we can get
    // it from some of the available parameters, we'll put it in.
    double frischGrid = 2098.998*millimeter;

    //PR(mTPCdb->frischGrid());
    //PR(z);
  // z is in tpc local sector coordinate system. z>=0;
   
   
    double tb = (//gTpcDbPtr->PadPlaneGeometry()->frischGrid()
		 frischGrid
		 +gTpcDbPtr->Electronics()->tZero() *gTpcDbPtr->SlowControlSim()->driftVelocity()
		 - z)
	/gTpcDbPtr->SlowControlSim()->driftVelocity();
    
  return((int)(tb/(mTimeBinWidth)));//time bin starts at 0,HL,9/1/99
}

//
// Rotation Matrices
//

StThreeVectorF
StTpcCoordinateTransform::rotateToLocal(const StThreeVectorF& a,
				     const int sector)
{
    // Should be replaced with Rotation class:
    //
    // define 2x2 rotation matrix
    //
    // ( cos Þ   sin Þ )
    // (-sin Þ   cos Þ )

  //   double beta = sector*M_PI/6;   //(30 degrees)
    
    //
    // In order to speed up the code, the Matrix constructors
    // have been moved to data Members and are initialized in
    // the constructor.  This has also meant the removal of
    // a lot of "const" because many functions now modify the
    // data members.  This is the old code:
    //
//     const int m = 2;  
//     const int n = 2;
//     StMatrix<double> m1(m,n,0);

//     // vector to be rotated
//     StMatrix<double> v1(2,1,0);
//     v1(1,1) = a.x();
//     v1(2,1) = a.y();  // z co-ordinate is immaterial

//     m1(1,1) = cos(beta);
//     m1(1,2) = -sin(beta);

//     m1(2,1) = -1*m1(1,2); // saves calculation sin(beta);
//     m1(2,2) = m1(1,1);    // saves calculation cos(beta);

//     PR(m1);
//     StMatrix<double> newMatrix = m1*v1;
//     PR(newMatrix);
    //
    // Now modify the data members instead:

    double     beta=   (sector>12)? 	
                      sector*M_PI/6 :
		     -sector*M_PI/6 ;   //(30 degrees)
    mRotation(1,1) =  cos(beta);
    mRotation(1,2) =  sin(beta);
    mRotation(2,1) = -1.*mRotation(1,2);   // saves calculation sin(beta);
    mRotation(2,2) =    mRotation(1,1);   // saves calculation cos(beta);

    mRotate(1,1) = a.x();
    mRotate(2,1) = a.y();  // z co-ordinate is immaterial

    mResult = mRotation*mRotate;
//     PR(mResult);
    
    return (sector>12)? (StThreeVectorF(mResult(1,1),mResult(2,1),-a.z()))
                        : (StThreeVectorF(mResult(1,1),mResult(2,1),a.z()));
}
 
StThreeVectorF 
StTpcCoordinateTransform::rotateFromLocal(const StThreeVectorF& a,
						     const int sector)
{
    //
    // define 2x2 rotation matrix
    //
    // ( cos Þ   sin Þ )
    // (-sin Þ   cos Þ )

  // double beta = (sector>12) ? (sector-12)*M_PI/6 : -sector*M_PI/6;   //(30 degrees)  NEGATIVE ANGLE!!!!!!!!
    //double beta = -sector*M_PI/6;   //(30 degrees)  NEGATIVE ANGLE!!!!!!!!

    //
    // See above for explanation.  All old code is as below
    //
//     const int m = 2;  
//     const int n = 2;
//     StMatrix<double> m1(m,n,0);

//     // vector to be rotated
//     StMatrix<double> v1(2,1,0);

//     v1(1,1) = a.x();
//     v1(2,1) = a.y();  // z co-ordinate is immaterial

//     idb << "Rotation angle is " << beta << endl;
//     m1(1,1) = cos(beta);
//     m1(1,2) = -sin(beta);

//     m1(2,1) = -1*m1(1,2); // saves calculation sin(beta);
//     m1(2,2) = m1(1,1);   // saves calculation cos(beta);

//     StMatrix<double> newMatrix = m1*v1;
//     PR(newMatrix);

    //
    // New code:
    // 
    double beta = (sector>12) ?
		-sector*M_PI/6 :
		sector*M_PI/6 ;   //(30 degrees)
    mRotation(1,1) =  cos(beta);
    mRotation(1,2) =  sin(beta);
    mRotation(2,1) = -1.*mRotation(1,2);   // saves calculation sin(beta);
    mRotation(2,2) =     mRotation(1,1);   // saves calculation cos(beta);

    mRotate(1,1) = a.x();
    mRotate(2,1) = a.y();  // z co-ordinate is immaterial
  
    mResult = mRotation*mRotate;
//     PR(mResult);
    return (sector>12) ? (StThreeVectorF(mResult(1,1),mResult(2,1),-a.z()))
                         : (StThreeVectorF(mResult(1,1),mResult(2,1),a.z()));
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
