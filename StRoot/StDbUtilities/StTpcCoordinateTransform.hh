/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.hh,v 1.12 2007/07/12 19:22:01 fisyak Exp $
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
 * Revision 1.12  2007/07/12 19:22:01  fisyak
 * Tpc Drift Velocity depends on West/East half
 *
 * Revision 1.11  2007/03/21 16:39:05  fisyak
 * TpcCoordinate transformation via TGeoHMatrix
 *
 * Revision 1.10  2005/07/06 19:10:34  fisyak
 * Add TpcCoordinate transormation classes to dictionary, use templated StThreeVector
 *
 * Revision 1.9  2004/06/05 23:31:09  fisyak
 * Add (sector,row) for Tpc Coordinate/Direction transformations; Change sign of t0zoffset correction (to be synch. with fcf)
 *
 * Revision 1.8  2004/03/05 17:22:54  fisyak
 * Add TPC transformations for direction, aligned sectors, protection in order to stay in the same sector when moving from/to Pad coordinates
 *
 * Revision 1.7  2001/05/22 22:32:14  hardtke
 * Add tpc global to local transformations
 *
 * Revision 1.6  2000/07/27 23:06:50  hardtke
 * Hui requested that all member functions be public -- probably bad programming, but TRS needs to be speeded up
 *
 * Revision 1.5  2000/05/25 20:51:31  hardtke
 * make z-to-time functions public, use correct t0s, get rid of spurious 0.5
 *
 * Revision 1.4  2000/04/13 22:57:53  calderon
 * use lookup table of sines and cosines instead of calculating them
 * each time
 *
 * Revision 1.3  2000/02/10 01:19:37  calderon
 * Tpc Local Sector Coordinate definitions where
 * y is up,
 * z points toward central membrane,
 * x is such that everything is righthanded
 * There are still some parameters that need to be taken from
 * database.
 *
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
// SCL
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#if 0
#include "StMatrix.hh"
#endif
#include "StTpcDb/StTpcDb.h"
#include "StObject.h"

//#define DEBUG_TPC 0
//#define idb if(DEBUG_TPC) cout
#include "StTpcPadCoordinate.hh"
#include "StTpcLocalCoordinate.hh"
#include "StTpcLocalSectorCoordinate.hh"
#include "StTpcLocalSectorAlignedCoordinate.hh"
#include "StGlobalCoordinate.hh"

#include "StTpcLocalDirection.hh"
#include "StTpcLocalSectorDirection.hh"
#include "StTpcLocalSectorAlignedDirection.hh"
#include "StGlobalDirection.hh"

class StTpcCoordinateTransform {//: public StObject {
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
  
  void  operator()(const              StTpcLocalCoordinate&, StTpcLocalSectorCoordinate&       );
  void  operator()(const        StTpcLocalSectorCoordinate&, StTpcLocalCoordinate&             );
  void  operator()(const        StTpcLocalSectorCoordinate&, StTpcLocalSectorAlignedCoordinate&);
  void  operator()(const StTpcLocalSectorAlignedCoordinate&, StTpcLocalSectorCoordinate&       );
  void  operator()(const StTpcLocalSectorAlignedCoordinate&, StTpcLocalCoordinate&             );
  void  operator()(const              StTpcLocalCoordinate&, StTpcLocalSectorAlignedCoordinate&);

  void  operator()(const               StTpcLocalDirection&, StTpcLocalSectorDirection&        );
  void  operator()(const         StTpcLocalSectorDirection&, StTpcLocalDirection&              );
  void  operator()(const         StTpcLocalSectorDirection&, StTpcLocalSectorAlignedDirection& );
  void  operator()(const  StTpcLocalSectorAlignedDirection&, StTpcLocalSectorDirection&        );
  void  operator()(const  StTpcLocalSectorAlignedDirection&, StTpcLocalDirection&              );
  void  operator()(const               StTpcLocalDirection&, StTpcLocalSectorAlignedDirection& );
// Tpc Local Sector <--> Global
    void  operator()(const  StTpcLocalSectorCoordinate&, StGlobalCoordinate&);
    void  operator()(const  StGlobalCoordinate& ,StTpcLocalSectorCoordinate&, int sector=0, int row = 0);
    void  operator()(const  StGlobalCoordinate& ,StTpcLocalSectorAlignedCoordinate&, int sector=0, int row = 0);
    void  operator()(const  StTpcLocalSectorDirection&, StGlobalDirection&);
    void  operator()(const  StGlobalDirection& ,StTpcLocalSectorDirection&, int sector, int row = 0);
    void  operator()(const  StGlobalDirection& ,StTpcLocalSectorAlignedDirection&, int sector, int row = 0);

    
// Internal TpcCoordinate <-->  Global Coordinate
    void  operator()(const StTpcLocalCoordinate&, StGlobalCoordinate&);
    void  operator()(const StGlobalCoordinate&, StTpcLocalCoordinate&, int sector=0, int row = 0);
    void  operator()(const StTpcLocalDirection&, StGlobalDirection&);
    void  operator()(const StGlobalDirection&, StTpcLocalDirection&, int sector, int row = 0);

//      Raw Data          <-->  Global Coordinate
    void  operator()(const StTpcPadCoordinate&, StGlobalCoordinate&);
    void  operator()(const StGlobalCoordinate&, StTpcPadCoordinate&, int sector = 0, int row = 0);

    StThreeVector<double> sector12Coordinate(StThreeVector<double>&, int*);
    StThreeVector<double> padCentroid(StTpcLocalSectorCoordinate&, int*, int*)  ;
  int      tBFromZ(const double, Int_t sector=24) const;
  double   zFromTB(const    int, Int_t sector=24) const;
    
    // Transformation Routines!!
    // Raw Data From tpc local Coordinates
  int      sectorFromCoordinate(const StThreeVector<double>&)      const;
  int      sectorFromCoordinate(const StTpcLocalCoordinate& a)       const {return sectorFromCoordinate(a.position());}
  int      sectorFromCoordinate(const StTpcLocalDirection&  a)       const {return sectorFromCoordinate(a.position());}
    // Raw Data (pad row timebin or drift L From tpc local sector Coordinates
    int      rowFromLocal(const StThreeVector<double>&)              const;
    int      padFromLocal(const StThreeVector<double>&, int)         const;
  int      padFromX(double x, int row) const; 
  int      rowFromLocal(const StTpcLocalSectorCoordinate& a)         const {return rowFromLocal(a.position());}
  int      padFromLocal(const StTpcLocalSectorCoordinate& a)         const {return padFromLocal(a.position().x(),a.fromRow());}
    // tpc local sector Coordinates from Raw Data
    StThreeVector<double> xyFromRaw(const StTpcPadCoordinate&)      ;
    double                yFromRow(const int)                  const;
    double                xFromPad(const int, const int)       const;
    
    // (3d)rotations   From means "From the TPC local  Coordinates to Tpc Local  Sector Coordinates "     
  //    "to" means " from Tpc local sector  Coordinates to  TPC local  Coordinates "
  // idir == 1 transformation for coordinate, idir != 1 transformation for direction
    StThreeVector<double> rotateToLocal(const StThreeVector<double>&, const int sector, const int dir = 1)  ;
    StThreeVector<double> rotateFromLocal(const StThreeVector<double>&, const int sector, const int dir = 1);

    // Utilities
    double      rad2deg(double)        const; //radians to degrees (should be in global?)
    int         nearestInteger(double) const;
#if 0
  const StMatrix<double>      &TpcToGlobalRotation() const {return *&mTpcToGlobalRotation;}
  const StMatrix<double>      &GlobalToTpcRotation() const {return *&mGlobalToTpcRotation;}
  const StThreeVector<double> &TpcPositionInGlobal() const {return *&mTpcPositionInGlobal;}
#endif      
private:
    double    mCosForSector[24];
    double    mSinForSector[24];
#if 0
    StMatrix<double>  mRotation;  // (2x2)
    StMatrix<double>  mRotate;    // (2x1)
    StMatrix<double>  mResult;    // (2x1)
    StMatrix<double>  mTpcToGlobalRotation; // (3X3)
    StMatrix<double>  mGlobalToTpcRotation; // (3X3)
    StThreeVector<double> mTpcPositionInGlobal; 
#endif
  double    mInnerPositionOffsetX[24]; // sector alignment
  double    mOuterPositionOffsetX[24]; // 
  double    mInnerRotation[24];        // rad
  double    mOuterRotation[24];        //
    StTpcDb*          gTpcDbPtr; 
    double            mTimeBinWidth;
    double            mInnerSectorzOffset; 
    double            mOuterSectorzOffset; 
    double            mDriftDistance; 
  //  ClassDef(StTpcCoordinateTransform,0) //
};

#endif



