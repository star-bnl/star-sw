/***********************************************************************
 *
 * $Id: StRichCoordinateTransform.h,v 1.1 2000/02/08 16:34:04 lasiuk Exp $
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
 * Revision 1.1  2000/02/08 16:34:04  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
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

#include "StMatrix.hh"
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
//#include "StMatrix.hh"

#include "StRichCoordinates.h"
    StRichCoordinateTransform(StRichGeometryDbInterface*);
    StRichCoordinateTransform(); // DO NOT CALL!
public:
    static StRichCoordinateTransform* getTransform(StRichGeometryDbInterface*);
    static StRichCoordinateTransform* getTransform(); // do not call!

    ~StRichCoordinateTransform();
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
    
protected:
    StRichCoordinateTransform(StRichGeometryDbInterface*);
    StRichCoordinateTransform(); // DO NOT CALL!
    double   row2QuadX(int,int)      const;
    double   pad2QuadZ(int,int)      const;
    int      quadX2Row(double,int)   const;
    int      quadZ2Pad(double, int)  const;
    int      nearestInteger(double)   const;
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

private:
    static StRichCoordinateTransform* mInstance;
    

    double mPadPitch;
    double mRowPitch;
    double mXGap;
    double mZQBound;
    //
    double mZLBound;
    double mXQBound;
    double mYQBound;
    double mXLBound;
    double mYLBound;
    // Survey
    double mSinB;
    double mRadialDistanceToRich;

    
};

#endif
