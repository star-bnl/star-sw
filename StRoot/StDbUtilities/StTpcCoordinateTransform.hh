/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.hh,v 1.15 2009/09/22 22:39:38 fine Exp $
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
 * Revision 1.15  2009/09/22 22:39:38  fine
 * fix the StThreeVector invocation
 *
 * Revision 1.14  2009/05/20 02:49:51  genevb
 * Introduce tpcPadrowT0 time offsets
 *
 * Revision 1.13  2008/05/27 14:26:40  fisyak
 * Use TChairs, absorb shift tau shift, introduce sector to sector time offset
 *
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
  StTpcCoordinateTransform(StTpcDb*);
  ~StTpcCoordinateTransform();
  //      Raw Data          <--> Tpc Local Sector Coordinates
  void  operator()(const StTpcLocalSectorCoordinate&, StTpcPadCoordinate&, Bool_t useT0=kTRUE, Bool_t useTau=kTRUE);
  void  operator()(const StTpcPadCoordinate&, StTpcLocalSectorCoordinate&, Bool_t useT0=kTRUE, Bool_t useTau=kTRUE);
  //      Raw Data          <--> Tpc Local  Coordinates
  void  operator()(const StTpcLocalCoordinate&, StTpcPadCoordinate&, Bool_t useT0=kTRUE, Bool_t useTau=kTRUE);
  void  operator()(const StTpcPadCoordinate&, StTpcLocalCoordinate&, Bool_t useT0=kTRUE, Bool_t useTau=kTRUE);
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
  void  operator()(const  StGlobalCoordinate& ,StTpcLocalSectorCoordinate&, Int_t sector=0, Int_t row = 0);
  void  operator()(const  StGlobalCoordinate& ,StTpcLocalSectorAlignedCoordinate&, Int_t sector=0, Int_t row = 0);
  void  operator()(const  StTpcLocalSectorDirection&, StGlobalDirection&);
  void  operator()(const  StGlobalDirection& ,StTpcLocalSectorDirection&, Int_t sector, Int_t row = 0);
  void  operator()(const  StGlobalDirection& ,StTpcLocalSectorAlignedDirection&, Int_t sector, Int_t row = 0);
  // Internal TpcCoordinate <-->  Global Coordinate
  void  operator()(const StTpcLocalCoordinate&, StGlobalCoordinate&);
  void  operator()(const StGlobalCoordinate&, StTpcLocalCoordinate&, Int_t sector=0, Int_t row = 0);
  void  operator()(const StTpcLocalDirection&, StGlobalDirection&);
  void  operator()(const StGlobalDirection&, StTpcLocalDirection&, Int_t sector, Int_t row = 0);
  //      Raw Data          <-->  Global Coordinate
  void  operator()(const StTpcPadCoordinate&, StGlobalCoordinate&, Bool_t useT0=kTRUE, Bool_t useTau=kTRUE);
  void  operator()(const StGlobalCoordinate&, StTpcPadCoordinate&, Int_t sector = 0, Int_t row = 0, Bool_t useT0=kTRUE, Bool_t useTau=kTRUE);
  StThreeVector<Double_t> sector12Coordinate(StThreeVector<Double_t>&, Int_t*);
  StThreeVector<Double_t> padCentroid(StTpcLocalSectorCoordinate&, Int_t*, Int_t*)  ;
  Float_t   tBFromZ(Double_t z, Int_t sector=24, Int_t row=45) const;
  Double_t  zFromTB(Float_t tb, Int_t sector=24, Int_t row=45) const;
  // Transformation Routines!!
  // Raw Data From tpc local Coordinates
  Int_t      sectorFromCoordinate(const StThreeVector<Double_t>&)    const;
  Int_t      sectorFromCoordinate(const StTpcLocalCoordinate& a)     const {return sectorFromCoordinate(a.position());}
  Int_t      sectorFromCoordinate(const StTpcLocalDirection&  a)     const {return sectorFromCoordinate(a.position());}
  // Raw Data (pad row timebin or drift L From tpc local sector Coordinates
  Int_t      rowFromLocal(const StThreeVector<Double_t>&)            const;
  Float_t    padFromLocal(const StThreeVector<Double_t>&, Int_t)     const;
  Float_t    padFromX(Double_t x, Int_t row)                         const; 
  Int_t      rowFromLocal(const StTpcLocalSectorCoordinate& a)       const {return rowFromLocal(a.position());}
  Float_t    padFromLocal(const StTpcLocalSectorCoordinate& a)       const {
                 StThreeVector<double> pad(a.position().x(),0,0);
                 return padFromLocal(pad,a.fromRow());
     }
  // tpc local sector Coordinates from Raw Data
  StThreeVector<Double_t> xyFromRaw(const StTpcPadCoordinate&);
  Double_t                yFromRow(Int_t row)                        const;
  Double_t                xFromPad(Int_t row, Float_t pad)           const;
  // (3d)rotations   From means "From the TPC local  Coordinates to Tpc Local  Sector Coordinates "     
  //    "to" means " from Tpc local sector  Coordinates to  TPC local  Coordinates "
  // idir == 1 transformation for coordinate, idir != 1 transformation for direction
  StThreeVector<Double_t> rotateToLocal(const StThreeVector<Double_t>&, Int_t sector, Int_t dir = 1)  ;
  StThreeVector<Double_t> rotateFromLocal(const StThreeVector<Double_t>&, Int_t sector, Int_t dir = 1);
  
  // Utilities
  Double_t      rad2deg(double)        const; //radians to degrees (should be in global?)
  Int_t         nearestInteger(double) const;
private:
  Double_t    mCosForSector[24];
  Double_t    mSinForSector[24];
  Double_t    mInnerPositionOffsetX[24]; // sector alignment
  Double_t    mOuterPositionOffsetX[24]; // 
  Double_t    mInnerRotation[24];        // rad
  Double_t    mOuterRotation[24];        //
  StTpcDb*    gTpcDbPtr; 
  Double_t    mTimeBinWidth;
  Double_t    mInnerSectorzOffset; 
  Double_t    mOuterSectorzOffset; 
  Double_t    mDriftDistance; 
  //  ClassDef(StTpcCoordinateTransform,0) //
};

#endif



