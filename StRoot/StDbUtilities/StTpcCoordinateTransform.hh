/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.hh,v 1.2 2000/02/02 23:01:38 calderon Exp $
 *
 * Author: brian made this on  Feb 6, 1998
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
 * $Log: StTpcCoordinateTransform.hh,v $
 * Revision 1.2  2000/02/02 23:01:38  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:08  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.5  1999/10/04 16:00:46  long
 * new coordinate system in the trs
 *
 * Revision 1.5  1999/10/01  17:15:00  Hui Long
 * New definition of the coordinate system according to Brian nad Thomas's
 * proposal
 * Revision 1.4  1999/02/18 21:17:42  lasiuk
 * instantiate with electronics db
 *
 * Revision 1.3  1999/02/16 23:28:46  lasiuk
 * matrix(3) is a data member to avoid constructor calls
 * protection against pad<1
 * const removed from several functions (because of matrix)
 *
 * Revision 1.2  1999/01/28 02:51:40  lasiuk
 * add ()localSector --> Raw
 * add ()localSector --> Local
 *
 * Revision 1.1  1998/11/10 17:12:05  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/01 16:21:05  lasiuk
 * remove 'St' from variable declarations
 * add set functions in local Coordinates
 *
 * Revision 1.4  1998/10/22 00:23:17  lasiuk
 * Oct 22
 *
 * Revision 1.3  1998/06/04 23:13:18  lasiuk
 * add sector12 coordinate transform as public member
 *
 * Revision 1.2  1998/05/25 17:00:31  lasiuk
 * pass databases instead of filenames
 * do not use hardcoded numbers
 *
 * Revision 1.1  1998/05/21 21:27:37  lasiuk
 * Initial revision
 *
 *
 ***********************************************************************/
#ifndef ST_COORDINATE_TRANSFORM_HH
#define ST_COORDINATE_TRANSFORM_HH

#include <stdlib.h>
//#include <unistd.h>
#ifndef ST_NO_EXCEPTIONS
//#include <stdexcept>
#endif

// SCL
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StMatrix.hh"

#include "StTpcDb/StTpcDb.h"

#define DEBUG_TPC 0
#define idb if(DEBUG_TPC) cout

class StGlobalCoordinate;
class StTpcLocalCoordinate;
class StTpcLocalSectorCoordinate;
class StTpcPadCoordinate;

class StTpcCoordinateTransform {
public:
  //StTpcCoordinateTransform(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*);
  StTpcCoordinateTransform(StTpcDb*);
    ~StTpcCoordinateTransform();
    //StTpcCoordinateTransform(const StTpcCoordinateTransform&);
    //StTpcCoordinateTransform& operator=(const StTpcCoordinateTransform&);
    
//      Raw Data          <--> Tpc Local Sector Coordinates
 
    void  operator()(const StTpcLocalSectorCoordinate&, StTpcPadCoordinate&);
    void  operator()(const StTpcPadCoordinate&, StTpcLocalSectorCoordinate&);
//      Raw Data          <--> Tpc Local  Coordinates
 
    void  operator()(const StTpcLocalCoordinate&, StTpcPadCoordinate&);
    void  operator()(const StTpcPadCoordinate&, StTpcLocalCoordinate&);
// Tpc Local Sector <--> TPC Local
  
    void  operator()(const  StTpcLocalCoordinate& ,StTpcLocalSectorCoordinate&);
    void  operator()(const StTpcLocalSectorCoordinate&, StTpcLocalCoordinate&);
// Tpc Local Sector <--> Global
    void  operator()(const StTpcLocalSectorCoordinate&, StGlobalCoordinate&);
    void  operator()(const  StGlobalCoordinate& ,StTpcLocalSectorCoordinate&);

    
// Internal TpcCoordinate <-->  Global Coordinate
    void  operator()(const StTpcLocalCoordinate&, StGlobalCoordinate&);
    void  operator()(const StGlobalCoordinate&, StTpcLocalCoordinate&);

//      Raw Data          <-->  Global Coordinate
    void  operator()(const StTpcPadCoordinate&, StGlobalCoordinate&);
    void  operator()(const StGlobalCoordinate&, StTpcPadCoordinate&);

    StThreeVector<double> sector12Coordinate(StThreeVector<double>&, int*);
    StThreeVector<double> padCentroid(StTpcLocalSectorCoordinate&, int*, int*)  ;
    
private:
    // Transformation Routines!!
    // Raw Data From tpc local Coordinates
    int      sectorFromCoordinate(const StTpcLocalCoordinate&)       const;
    int      sectorFromCoordinate(const StThreeVector<double>&)      const;
    // Raw Data (pad row timebin or drift L From tpc local sector Coordinates
    int      rowFromLocal(const StThreeVector<double>&)              const;
    int      padFromLocal(const StThreeVector<double>&, const int)   const;
    int      tBFromZ(const double)                                   const;

    // tpc local sector Coordinates from Raw Data
    StThreeVector<double> xyFromRaw(const StTpcPadCoordinate&)      ;
    double                yFromRow(const int)                  const;
    double                xFromPad(const int, const int)       const;
    double                zFromTB(const int)                   const;
    
    // (3d)rotations   From means "From the TPC local  Coordinates to Tpc Local  Sector Coordinates "     
  //    "to" means " from Tpc local sector  Coordinates to  TPC local  Coordinates "
    StThreeVector<double> rotateToLocal(const StThreeVector<double>&, const int)  ;
    StThreeVector<double> rotateFromLocal(const StThreeVector<double>&, const int);

    // Utilities
    double      rad2deg(double)        const; //radians to degrees (should be in global?)
    int         nearestInteger(double) const;

private:
    StMatrix<double>  mRotation;  // (2x2)
    StMatrix<double>  mRotate;    // (2x1)
    StMatrix<double>  mResult;    // (2x1)
    
    //StTpcGeometry    *mTPCdb;         // singleton class
    //StTpcSlowControl *mSCdb;          // singleton class
    //StTpcElectronics *mElectronicsDb; // singleton class
    StTpcDb*          gTpcDbPtr; 
    double            mTimeBinWidth;
    double            mInnerSectorzOffset; //These are in temporarily,
    double            mOuterSectorzOffset; //until StTpcDb has them.
};

#endif
