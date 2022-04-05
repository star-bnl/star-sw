/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.hh,v 1.29 2018/10/17 20:45:24 fisyak Exp $
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
 * Revision 1.29  2018/10/17 20:45:24  fisyak
 * Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
 *
 * Revision 1.27  2018/08/07 03:43:37  fisyak
 * iTPC corrections
 *
 * Revision 1.25  2018/06/21 01:46:56  perev
 * iTPCheckIn
 *
 * Revision 1.22.10.1  2018/02/16 22:14:56  perev
 * iTPC
 * Revision 1.24  2018/06/07 04:28:42  genevb
 * Explicit include for TMath needed
 *
 * Revision 1.23  2018/04/11 02:43:44  smirnovd
 * StTpcCoordinateTransform: Extend interface to accept TPC sector + use padConfig
 *
 * Revision 1.22  2014/06/26 21:29:27  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.21  2012/10/23 20:13:17  fisyak
 * Move xFromPad from h- to cxx-file
 *
 * Revision 1.20  2012/09/13 20:57:28  fisyak
 * Corrections for iTpx
 *
 * Revision 1.19  2012/05/07 14:38:41  fisyak
 * Remvoe hardcoded separation between Inner and Outer Sectors
 *
 * Revision 1.18  2011/01/18 14:34:28  fisyak
 * Clean up TpcDb interfaces and Tpc coordinate transformation
 *
 * Revision 1.17  2009/11/02 17:32:25  fisyak
 * remove defaults in Tpc Coordinate transformations
 *
 * Revision 1.16  2009/09/23 23:30:14  fisyak
 * Follow up Valery's corrections
 *
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
#include "TMath.h"
// SCL
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
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
  // pad              => sector12       =>   subsector => sector => tpc      => global
  // TpcPadCoordinate => TpcSectL => TpcSectLAligned => TpcLocal => Global
class StTpcCoordinateTransform {//: public StObject {
public:
  StTpcCoordinateTransform(StTpcDb* globalDbPointer=0);
  ~StTpcCoordinateTransform() {}
  //      Raw Data          <--> Tpc Local Sector Coordinates
  void  operator()(const StTpcLocalSectorCoordinate& a, StTpcPadCoordinate& b,Bool_t useT0=kFALSE, Bool_t useTau=kTRUE);
  void  operator()(const StTpcPadCoordinate& a, StTpcLocalSectorCoordinate& b,Bool_t useT0=kFALSE, Bool_t useTau=kTRUE);
  //      Raw Data          <--> Tpc Local  Coordinates
  void  operator()(const StTpcLocalCoordinate& a, StTpcPadCoordinate& b,Bool_t useT0=kFALSE, Bool_t useTau=kTRUE) 
  {StTpcLocalSectorCoordinate c; this->operator()(a,c); this->operator()(c,b,useT0,useTau);}
  void  operator()(const StTpcPadCoordinate& a, StTpcLocalCoordinate& b,Bool_t useT0=kFALSE, Bool_t useTau=kTRUE)
  {StTpcLocalSectorCoordinate c; this->operator()(a,c,useT0,useTau); this->operator()(c,b);}
  // Tpc Local Sector <--> TPC Local
  void  operator()(const        StTpcLocalSectorCoordinate& a, StTpcLocalCoordinate& b           );
//   { Double_t xyzS[3] = {a.position().x(), a.position().y(), 0.};
//     StTpcDb::instance()->Pad2Tpc(a.sector(),a.row()).LocalToMaster(xyzS,b.position().xyz()); 
//     b.position().setZ(b.position().z() + a.position().z());
//     b.setSector(a.sector()); b.setRow(a.row());}
  void  operator()(const        StTpcLocalSectorCoordinate& a, StTpcLocalSectorAlignedCoordinate& b) {b = a;}
  void  operator()(const        StTpcLocalSectorDirection&  a, StTpcLocalDirection&               b)
  {StTpcDb::instance()->Pad2Tpc(a.sector(),a.row()).LocalToMasterVect(a.position().xyz(),b.position().xyz()); b.setSector(a.sector()); b.setRow(a.row());}
  void  operator()(const        StTpcLocalSectorDirection& a, StTpcLocalSectorAlignedDirection& b)   {b = a;}
  void  operator()(const        StTpcLocalSectorCoordinate& a, StGlobalCoordinate& b)  
  { StTpcLocalCoordinate c;    this->operator()(a,c);    this->operator()(c,b);  }
  void  operator()(const  StTpcLocalSectorDirection& a, StGlobalDirection& b) 
  {StTpcDb::instance()->Pad2Glob(a.sector(),a.row()).LocalToMasterVect(a.position().xyz(),b.position().xyz());}
  void  operator()(const              StTpcLocalCoordinate& a, StTpcLocalSectorCoordinate& b     ); 
//   { Double_t xyzS[3] = {a.position().x(), a.position().y(), 0.};
//     StTpcDb::instance()->Pad2Tpc(a.sector(),a.row()).MasterToLocal(xyzS,b.position().xyz()); b.setSector(a.sector()); b.setRow(a.row());
//     b.position().setZ(b.position().z() + a.position().z());
//   }
  void  operator()(const               StTpcLocalDirection& a, StTpcLocalSectorDirection& b      )
  {StTpcDb::instance()->Pad2Tpc(a.sector(),a.row()).MasterToLocalVect(a.position().xyz(),b.position().xyz()); b.setSector(a.sector()); b.setRow(a.row());}
  void  operator()(const               StGlobalCoordinate& a,StTpcLocalSectorCoordinate& b,Int_t sector, Int_t row)
  { StTpcLocalCoordinate c;
    this->operator()(a,c,sector,row);
    this->operator()(c,b);
  }
  void  operator()(const  StGlobalDirection& a,StTpcLocalSectorDirection& b,Int_t sector, Int_t row)
  {StTpcDb::instance()->Pad2Glob(sector,row).MasterToLocalVect(a.position().xyz(),b.position().xyz()); b.setSector(sector); b.setRow(row);}
  // Internal TpcCoordinate <-->  Global Coordinate
  void  operator()(const StTpcLocalCoordinate& a, StGlobalCoordinate& b)
  {StTpcDb::instance()->Tpc2GlobalMatrix().LocalToMaster(a.position().xyz(),b.position().xyz());}
  void  operator()(const StGlobalCoordinate& a, StTpcLocalCoordinate& b,Int_t sector, Int_t row)
  {StTpcDb::instance()->Tpc2GlobalMatrix().MasterToLocal(a.position().xyz(),b.position().xyz()); b.setSector(sector); b.setRow(row);}
  void  operator()(const StTpcLocalDirection& a, StGlobalDirection& b)
  {StTpcDb::instance()->Tpc2GlobalMatrix().LocalToMasterVect(a.position().xyz(),b.position().xyz());}
  void  operator()(const StGlobalDirection& a, StTpcLocalDirection& b,Int_t sector, Int_t row)
  {StTpcDb::instance()->Tpc2GlobalMatrix().MasterToLocalVect(a.position().xyz(),b.position().xyz()); b.setSector(sector); b.setRow(row);}
  //      Raw Data          <-->  Global Coordinate
  void  operator()(const StTpcPadCoordinate& a, StGlobalCoordinate& b,Bool_t useT0=kFALSE, Bool_t useTau=kTRUE) 
  {StTpcLocalCoordinate c; this->operator()(a,c,useT0,useTau); this->operator()(c,b);}
  void  operator()(const StGlobalCoordinate& a, StTpcPadCoordinate& b,Int_t sector, Int_t row, Bool_t useT0=kFALSE, Bool_t useTau=kTRUE)
  {StTpcLocalCoordinate c; this->operator()(a,c,sector,row); this->operator()(c,b,useT0,useTau);}
  Double_t   tBFromZ(Double_t z, Int_t sector, Int_t row, Int_t pad = 0) const;
  Double_t  zFromTB(Double_t tb, Int_t sector, Int_t row, Int_t pad = 0) const;
  // Transformation Routines!!
  // Raw Data (pad row timebin or drift L From tpc local sector Coordinates
  static Int_t       rowFromLocalY(Double_t y, Int_t sector);
  static Int_t       rowFromLocal(const StThreeVector<Double_t>& a, Int_t sector)            {return rowFromLocalY(a.y(), sector);}
  Double_t    padFromLocal(const StThreeVector<Double_t>& a, Int_t sector, Int_t row)  const {return padFromX(a.x(), sector, row);}
  Double_t    padFromX(Double_t x, Int_t sector, Int_t row)                        const; 
  Int_t       rowFromLocal(const StTpcLocalSectorCoordinate& a)      const {return rowFromLocal(a.position(),a.sector());}
  Double_t    padFromLocal(const StTpcLocalSectorCoordinate& a)      const {return padFromLocal(a.position(),a.sector(),a.row());}
  // tpc local sector Coordinates from Raw Data
  StThreeVector<Double_t> xyFromRow(const StTpcPadCoordinate& a) {return StThreeVector<Double_t> (xFromPad(a.sector(),a.row(),a.pad()),yFromRow(a.sector(),a.row()),0);}
  Double_t                yFromRow(Int_t sector, Int_t row)                        const {return (St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,row));}
  Double_t                xFromPad(Int_t sector, Int_t row, Double_t pad)          const;
// sector from Tpc local coordinates
  Int_t sectorFromCoordinate(const StThreeVector<double>& a) const{
    Double_t angle = TMath::RadToDeg()*TMath::ATan2(a.y(),a.x());
    if(angle<0) angle+= 360;
    Int_t sectorNumber= (int)( (angle+15)/30);
    if(a.z()>0){sectorNumber=15-sectorNumber; if(sectorNumber> 12)sectorNumber-=12;}
    else       {sectorNumber+=9;              if(sectorNumber<=12)sectorNumber+=12;}
    return sectorNumber;
  }
private:
  Double_t    mTimeBinWidth;
  Double_t    mInnerSectorzOffset; 
  Double_t    mOuterSectorzOffset; 
#if 0
  Double_t    mInnerSectorzOffset_West; 
  Double_t    mOuterSectorzOffset_West; 
#endif
  Int_t       mNoOfInnerRows;
  Int_t       mNoOfRows;
  
  //  ClassDef(StTpcCoordinateTransform,0) //
};

#endif



