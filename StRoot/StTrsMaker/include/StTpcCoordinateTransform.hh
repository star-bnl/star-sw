/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.hh,v 1.1 1998/11/10 17:12:05 fisyak Exp $
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
 * Revision 1.1  1998/11/10 17:12:05  fisyak
 * Put Brian trs versin into StRoot
 *
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

#include "StCoordinates.hh"        // coordinate definitions
#include "StTpcGeometry.hh"        // Abstract (never changes)
#include "StTpcSlowControl.hh"     // Abstract (never changes)

#define DEBUG_TPC 0
#define idb if(DEBUG_TPC) cout

class StTpcCoordinateTransform {
public:
    //StTpcCoordinateTransform& operator=(const StTpcCoordinateTransform&);
    
    void  operator()(const StTpcLocalSectorCoordinate&, StTpcLocalCoordinate&) const;
    void  operator()(const StTpcLocalSectorCoordinate&, StTpcPadCoordinate&) const;
    void  operator()(const StTpcLocalCoordinate&, StTpcPadCoordinate&);

    void  operator()(const StTpcLocalCoordinate&, StGlobalCoordinate&) const;
    void  operator()(const StGlobalCoordinate&, StTpcLocalCoordinate&) const;
    void  operator()(const StTpcLocalSectorCoordinate&, StTpcPadCoordinate&);
    
    void  operator()(const StTpcPadCoordinate&, StGlobalCoordinate&)   const;
    void  operator()(const StGlobalCoordinate&, StTpcPadCoordinate&)   const;
    void  operator()(const StGlobalCoordinate&, StTpcLocalCoordinate&);
    StThreeVector<double> sector12Coordinate(StThreeVector<double>&, int*) const;
    StThreeVector<double> padCentroid(StTpcLocalCoordinate&, int*, int*)   const;
    void  operator()(const StTpcPadCoordinate&, StGlobalCoordinate&);
    void  operator()(const StGlobalCoordinate&, StTpcPadCoordinate&);

    StThreeVector<double> sector12Coordinate(StThreeVector<double>&, int*);
    StThreeVector<double> padCentroid(StTpcLocalCoordinate&, int*, int*)  ;
    
private:
    // Transformation Routines!!
    // Raw Data From Coordinates
    int      sectorFromCoordinate(const StTpcLocalCoordinate&)       const;
    int      sectorFromCoordinate(const StThreeVector<double>&)      const;
    StThreeVector<double> xyFromRaw(const StTpcPadCoordinate&) const;
    int      padFromLocal(const StThreeVector<double>&, const int)   const;
    int      tBFromZ(const double)                                   const;

    // Coordinates from Raw Data
    StThreeVector<double> xyFromRaw(const StTpcPadCoordinate&)      ;
    StThreeVector<double> rotateToLocal(const StThreeVector<double>&, const int)    const;
    StThreeVector<double> rotateFromLocal(const StThreeVector<double>&, const int)  const;
    double                zFromTB(const int)                   const;
    
    // rotations
    StThreeVector<double> rotateToLocal(const StThreeVector<double>&, const int)  ;
    StThreeVector<double> rotateFromLocal(const StThreeVector<double>&, const int);

    StTpcGeometry    *mTPCdb;  // singleton class
    StTpcSlowControl *mSCdb;   // singleton class
    StMatrix<double>  mRotate;    // (2x1)
    StMatrix<double>  mResult;    // (2x1)
    
    StTpcGeometry    *mTPCdb;     // singleton class
    StTpcSlowControl *mSCdb;      // singleton class
};

#endif
