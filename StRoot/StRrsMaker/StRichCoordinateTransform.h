/***********************************************************************
 *
 * $Id: StRichCoordinateTransform.h,v 1.4 2000/03/17 14:54:19 lasiuk Exp $
 *
 * Author: brian made this on Jan 27, 2000
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
 * $Log: StRichCoordinateTransform.h,v $
 * Revision 1.4  2000/03/17 14:54:19  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.3  2000/03/12 22:20:24  lasiuk
 * make into a singleton class
 * incliniation angle stored as data members
 *
 * Revision 1.2  2000/02/08 23:48:19  lasiuk
 * remove matrix from SCL
 *
 * Revision 1.1  2000/02/08 16:34:04  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 ***********************************************************************/
#ifndef ST_RICH_COORDINATE_TRANSFORM_HH
#define ST_RICH_COORDINATE_TRANSFORM_HH

#include <stdlib.h>
//#include <unistd.h>
#ifndef ST_NO_EXCEPTIONS
//#include <stdexcept>
#endif

// SCL
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
//#include "StMatrix.hh"

#include "StRichCoordinates.h"
#include "StRichGeometryDbInterface.h"

class StRichCoordinateTransform {
public:
    static StRichCoordinateTransform* getTransform(StRichGeometryDbInterface*);
    static StRichCoordinateTransform* getTransform(); // do not call!

    virtual ~StRichCoordinateTransform();
    //StRichCoordinateTransform(const StRichCoordinateTransform&);
    //StRichCoordinateTransform& operator=(const StRichCoordinateTransform&);
    
//     Raw Data      <-->  Quadrant RichCoordinate
    void  operator()(const StRichRawCoordinate&, StRichQuadrantCoordinate&);
    void  operator()(const StRichQuadrantCoordinate&, StRichRawCoordinate&);

//     Rich Quadrant <-->  Rich Local
    void  operator()(const StRichQuadrantCoordinate&, StRichLocalCoordinate&);
    void  operator()(const StRichLocalCoordinate&, StRichQuadrantCoordinate&);
    
//     Rich Local    <-->  STAR Global Coordinate
    void  operator()(const StRichLocalCoordinate&, StGlobalCoordinate&);
    void  operator()(const StGlobalCoordinate&, StRichLocalCoordinate&);

//     Raw Data      <-->  Rich Local
    void  operator()(const StRichRawCoordinate&, StRichLocalCoordinate&);
    void  operator()(const StRichLocalCoordinate&, StRichRawCoordinate&);

//     Rich Quadrant <-->  Global Coordinate
    void  operator()(const StRichQuadrantCoordinate&, StGlobalCoordinate&);
    void  operator()(const StGlobalCoordinate&, StRichQuadrantCoordinate&);
    
//     Raw Data     <-->  Global Coordinate
    void  operator()(const StRichRawCoordinate&, StGlobalCoordinate&);
    void  operator()(const StGlobalCoordinate&, StRichRawCoordinate&);

public:
    //
    // Routines that are public to save time from doing a complete
    // transformation.
    int      whichQuadrant(const StRichRawCoordinate&)   const;
    int      whichQuadrant(const StRichLocalCoordinate&) const;

protected:
    StRichCoordinateTransform(StRichGeometryDbInterface*);
    StRichCoordinateTransform(); // DO NOT CALL!

private:
    // Transformation Routines!!
    // Raw Data From Coordinates
    double   quadX2Pad(double, int)  const;
    double   row2QuadY(double, int)      const;
    double   pad2QuadX(double, int)      const;
    double   quadY2Row(double, int)   const;
    //int      nearestInteger(double)   const;

    // only called if RICH_COORDINATE_BOUNDS_CHECK
    // macro is defined
    int     checkRawBounds(const StRichRawCoordinate&)           const;
    int     checkQuadrantBounds(const StRichQuadrantCoordinate&) const;
    int     checkLocalBounds(const StRichLocalCoordinate&)       const;
private:
    //StMatrix<double>  mRotation;  // (2x2)
    //StMatrix<double>  mRotate;    // (2x1)
    //StMatrix<double>  mResult;    // (2x1)
    
    StRichGeometryDbInterface    *mGeomDb;

private:
    static StRichCoordinateTransform* mInstance;
    
    int mNumberOfPadsInAQuadrantRow;
    int mNumberOfRowsInAQuadrantColumn;
    double mPadPitch;
    double mRowPitch;
    double mXGap;
    double mYGap;
    //
    // For Bounds check
    double mXQBound;
    double mYQBound;
    double mXLBound;
    double mYLBound;
    
    //
    // Survey
    double mInclinationAngle;
    double mCosB;
    double mSinB;
    double mRadialDistanceToRich;

    
};

#endif
