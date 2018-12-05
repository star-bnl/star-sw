/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.cc,v 1.52 2018/12/05 14:56:28 fisyak Exp $
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
 * Revision 1.52  2018/12/05 14:56:28  fisyak
 * Fix a bug with radial row position
 *
 * Revision 1.51  2018/12/04 19:38:07  fisyak
 * Add J.Thomas fix for iTPC survey
 *
 * Revision 1.50  2018/10/17 20:45:24  fisyak
 * Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
 *
 * Revision 1.48  2018/08/07 03:43:37  fisyak
 * iTPC corrections
 *
 * Revision 1.46  2018/06/21 01:46:54  perev
 * iTPCheckIn
 *
 * Revision 1.42.6.3  2018/05/28 23:56:26  perev
 * Add backward compatibility
 *
 * Revision 1.42.6.2  2018/05/02 19:41:33  perev
 * Supress some Irakli correction
 *
 * Revision 1.42  2015/07/19 22:20:42  fisyak
 * Add recalculation of pad row during transformation
 *
 * Revision 1.41  2014/07/01 20:29:02  fisyak
 * Clean up
 *
 * Revision 1.40  2014/06/26 21:29:27  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.39  2012/10/23 20:13:17  fisyak
 * Move xFromPad from h- to cxx-file
 *
 * Revision 1.38  2012/09/13 20:57:28  fisyak
 * Corrections for iTpx
 *
 * Revision 1.37  2012/05/07 14:38:41  fisyak
 * Remvoe hardcoded separation between Inner and Outer Sectors
 *
 * Revision 1.36  2011/01/18 14:34:28  fisyak
 * Clean up TpcDb interfaces and Tpc coordinate transformation
 *
 * Revision 1.35  2009/11/02 17:32:25  fisyak
 * remove defaults in Tpc Coordinate transformations
 *
 * Revision 1.34  2009/05/20 02:49:51  genevb
 * Introduce tpcPadrowT0 time offsets
 *
 * Revision 1.33  2009/03/16 14:23:59  fisyak
 * Use StDetectorDb chairs for TpcGlobalPosition and TpcSectorPosition
 *
 * Revision 1.32  2008/12/23 17:27:57  fisyak
 * Use tpcT0 chair, use sector/row in global => local transformation
 *
 * Revision 1.31  2008/06/04 19:18:11  fisyak
 * Account sector t0 shift only once
 *
 * Revision 1.30  2008/05/27 14:26:40  fisyak
 * Use TChairs, absorb shift tau shift, introduce sector to sector time offset
 *
 * Revision 1.29  2007/07/12 19:22:00  fisyak
 * Tpc Drift Velocity depends on West/East half
 *
 * Revision 1.28  2007/05/17 15:28:57  fisyak
 * Replace cout and cerr with Loggger
 *
 * Revision 1.27  2007/03/21 16:39:04  fisyak
 * TpcCoordinate transformation via TGeoHMatrix
 *
 * Revision 1.26  2005/07/06 19:10:34  fisyak
 * Add TpcCoordinate transormation classes to dictionary, use templated StThreeVector
 *
 * Revision 1.25  2004/06/05 23:31:09  fisyak
 * Add (sector,row) for Tpc Coordinate/Direction transformations; Change sign of t0zoffset correction (to be synch. with fcf)
 *
 * Revision 1.24  2004/03/10 20:30:39  fisyak
 * Comment out check that Z is in fid. volume
 *
 * Revision 1.23  2004/03/05 17:22:54  fisyak
 * Add TPC transformations for direction, aligned sectors, protection in order to stay in the same sector when moving from/to Pad coordinates
 *
 * Revision 1.22  2004/01/14 22:39:08  fisyak
 *  unsigned int => size_t to make alpha happy
 *
 * Revision 1.21  2001/05/23 00:09:55  hardtke
 * Add error message if rotation matrix inversion fails
 *
 * Revision 1.20  2001/05/22 22:32:14  hardtke
 * Add tpc global to local transformations
 *
 * Revision 1.19  2000/12/05 17:54:23  hardtke
 * Fix bug in sectorFromCoordinate
 *
 * Revision 1.18  2000/06/24 00:20:35  hardtke
 * remove shaping time from time-to-z conversion
 *
 * Revision 1.17  2000/06/06 18:17:49  calderon
 * change exit to assert
 *
 * Revision 1.16  2000/05/31 19:30:38  hardtke
 * lannys modification to t0 definitions
 *
 * Revision 1.15  2000/05/25 20:51:31  hardtke
 * make z-to-time functions public, use correct t0s, get rid of spurious 0.5
 *
 * Revision 1.14  2000/04/17 20:14:42  calderon
 * fix bug, did not remove all use of matrices in last step of
 * rotation from local sector -> local.
 *
 * Revision 1.13  2000/04/13 22:57:53  calderon
 * use lookup table of sines and cosines instead of calculating them
 * each time
 *
 * Revision 1.12  2000/04/05 23:00:55  calderon
 * Use the outer sector edge as the boundary between charge going to
 * padrow 13 or 14.
 *
 * Revision 1.11  2000/04/05 20:00:19  hardtke
 * check for physical sector number before getting t0 table
 *
 * Revision 1.10  2000/04/05 13:58:13  hardtke
 * correct mistake in drift velocity units
 *
 * Revision 1.9  2000/04/04 20:32:27  hardtke
 * use correct drift velocity from database in time to z conversion
 *
 * Revision 1.8  2000/04/03 16:23:51  calderon
 * Fix bug in rowFromLocal.  Boundary btw inner and outer sector is now
 * taken as midpoint of last inner row (plus half its row pitch) and
 * first outer row (minus half its row pitch).
 *
 * Revision 1.7  2000/03/30 17:03:24  hardtke
 * add pad-by-pad t0 offsets to z calculation
 *
 * Revision 1.6  2000/02/24 18:20:58  hardtke
 * use drift distance and offsets from the database
 *
 * Revision 1.5  2000/02/23 14:52:59  hardtke
 * fix StTpcLocalSectorCoordinate to StTpcLocalCoordinate conversion
 *
 * Revision 1.4  2000/02/10 01:19:37  calderon
 * Tpc Local Sector Coordinate definitions where
 * y is up,
 * z points toward central membrane,
 * x is such that everything is righthanded
 * There are still some parameters that need to be taken from
 * database.
 *
 * Revision 1.3  2000/02/02 23:01:38  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.2  1999/12/03 00:50:31  calderon
 * Using StTpcDb (there are still problems with SlowControl parameters).
 *
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
 * xyFromRow is changed to take into account the inversion.
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
#include "StMessMgr.h"
#include "StDetectorDbMaker/St_tpcPadrowT0C.h"
#include "StDetectorDbMaker/St_tpcSectorT0offsetC.h"
#include "StDetectorDbMaker/St_tpcRDOT0offsetC.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0BC.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "StDetectorDbMaker/St_iTPCSurveyC.h"
#include "TMath.h"
#include "StThreeVectorD.hh"
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using namespace units;
#endif
static Int_t _debug = 0;
StTpcCoordinateTransform::StTpcCoordinateTransform(StTpcDb* /* globalDbPointer */)
 {
    if (St_tpcPadConfigC::instance() 
	&& StTpcDb::instance()->Electronics() 
#if 0
	&& StTpcDb::instance()->GlobalPosition()
#endif
	) {
	mTimeBinWidth = 1./StTpcDb::instance()->Electronics()->samplingFrequency();
        mInnerSectorzOffset = StTpcDb::instance()->Dimensions()->zInnerOffset();
        mOuterSectorzOffset = StTpcDb::instance()->Dimensions()->zOuterOffset();
#if 0
        mInnerSectorzOffset_West = StTpcDb::instance()->Dimensions()->zInnerOffset_West();
        mOuterSectorzOffset_West = StTpcDb::instance()->Dimensions()->zOuterOffset_West();
#endif
    }
    else {
	gMessMgr->Error() << "StTpcDb IS INCOMPLETE! Cannot contstruct Coordinate transformation." << endm;
	assert(St_tpcPadConfigC::instance());
	assert(StTpcDb::instance()->Electronics());
#if 0
        assert(StTpcDb::instance()->GlobalPosition());
#endif
    }
}
//________________________________________________________________________________
//      Local Sector Coordnate    <->  Tpc Raw Pad Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate& a, StTpcPadCoordinate& b, Bool_t useT0, Bool_t useTau)
{ // useT0 = kTRUE for pad and kFALSE for cluster, useTau = kTRUE for data cluster and  = kFALSE for MC
  Int_t sector = a.fromSector();
  Int_t row    = a.fromRow();
  if (row < 1 || row > St_tpcPadConfigC::instance()->numberOfRows(sector)) row    = rowFromLocal(a);
    
  Double_t probablePad = padFromLocal(a);
#if 0 /* Don't apply zOffSet for prompt hits */
  Double_t zoffset = 0; // Don't apply zOffSet for prompt hits
  if (a.position().z() < 3) zoffset = (row>St_tpcPadConfigC::instance()->innerPadRows(sector)) ? mOuterSectorzOffset :mInnerSectorzOffset;
#else
  Double_t                  zoffset = (row>St_tpcPadConfigC::instance()->innerPadRows(sector)) ? mOuterSectorzOffset :mInnerSectorzOffset;
#endif /* Don't apply zOffSet for prompt hits */
#if 0
  if (sector <= 12)         zoffset+= (row>St_tpcPadConfigC::instance()->innerPadRows(sector)) ? mOuterSectorzOffset_West :mInnerSectorzOffset_West;
#endif
  Double_t t0offset = (useT0 && sector>=1&&sector<=24) ? St_tpcPadGainT0BC::instance()->T0(sector,row,TMath::Nint (probablePad)) : 0;
  t0offset *= mTimeBinWidth;
  if (! useT0 && useTau) // for cluster
    t0offset -= 3.0 * St_tss_tssparC::instance()->tau();   // correct for convolution lagtime
  Double_t t0zoffset = t0offset*StTpcDb::instance()->DriftVelocity(sector)*1e-6;
  Double_t tb = tBFromZ(a.position().z()+zoffset-t0zoffset,sector,row,probablePad);
  b = StTpcPadCoordinate(sector, row, probablePad, tb);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a,  StTpcLocalSectorCoordinate& b, Bool_t useT0, Bool_t useTau) 
{ // useT0 = kTRUE for pad and kFALSE for cluster, useTau = kTRUE for data cluster and = kFALSE for MC
  StThreeVector<double>  tmp=xyFromRow(a);
  Int_t sector = a.sector();
#if 0 /* Don't apply zOffSet for prompt hits */
  Double_t zoffset= 0; // Don't apply zOffSet for prompt hits
  if (a.timeBucket() > 6) zoffset =  (a.row()>St_tpcPadConfigC::instance()->innerPadRows(sector)) ? mOuterSectorzOffset : mInnerSectorzOffset;
#else
  Double_t                zoffset =  (a.row()>St_tpcPadConfigC::instance()->innerPadRows(sector)) ? mOuterSectorzOffset : mInnerSectorzOffset;
#endif /* Don't apply zOffSet for prompt hits */
#if 0
  if (a.sector() <= 12)         zoffset+= (a.row() > St_tpcPadConfigC::instance()->innerPadRows(sector)) ? mOuterSectorzOffset_West :mInnerSectorzOffset_West;
#endif
  Double_t t0offset = useT0 ? St_tpcPadGainT0BC::instance()->T0(a.sector(),a.row(),TMath::Nint(a.pad())) : 0;
  t0offset *= mTimeBinWidth;
  if (! useT0 && useTau) // for cluster
    t0offset -= 3.0 * St_tss_tssparC::instance()->tau();   // correct for convolution lagtime
  Double_t t0zoffset = t0offset*StTpcDb::instance()->DriftVelocity(a.sector())*1e-6;
  //t0 offset -- DH  27-Mar-00
  Double_t z = zFromTB(a.timeBucket(),a.sector(),a.row(),a.pad())-zoffset+t0zoffset;
  tmp.setZ(z);
  b = StTpcLocalSectorCoordinate(tmp,a.sector(),a.row());
}
//________________________________________________________________________________
Double_t StTpcCoordinateTransform::padFromX(Double_t x, Int_t sector, Int_t row) const {
  if (row > St_tpcPadConfigC::instance()->numberOfRows(sector)) row = St_tpcPadConfigC::instance()->numberOfRows(sector);
  Double_t pitch = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ?
    St_tpcPadConfigC::instance()->innerSectorPadPitch(sector) :
    St_tpcPadConfigC::instance()->outerSectorPadPitch(sector);
  // x coordinate in sector 12
  Int_t npads = St_tpcPadConfigC::instance()->numberOfPadsAtRow(sector,row);
  Double_t xL = x;
  Int_t NiRows = St_tpcPadConfigC::instance()->numberOfInnerRows(sector);
  if (NiRows != 13 && row <= NiRows) {
    // iTPC Survey, see  Jim Thomas comments in StTpcCoordinateTransform::xFromPad
    Double_t yRef = St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,NiRows) + 0.565;
    Double_t xHit = xL;
    Double_t yHit = St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,row) - yRef;
    St_iTPCSurveyC *sur = St_iTPCSurveyC::instance();
    Double_t dx = sur->dx(sector-1);
//  Double_t dy = sur->dy(sector-1);
    Double_t Xscale = sur->ScaleX(sector-1);
//  Double_t Yscale = sur->ScaleY(sector-1);
    Double_t theta  = sur->Angle(sector-1);
             xL = xHit*(1. - Xscale) - dx + theta*yHit;
//  Double_t yL = yHit*(1. - Yscale) - dy - theta*xHit + yRef;
  }
  Double_t probablePad = (npads+1.)/2. - xL/pitch;
  // CAUTION: pad cannot be <1
  if(probablePad<0.500001) {
    probablePad=0.500001;
  }
  if (_debug) {
    cout << "StTpcCoordinateTransform::padFromX(" << x << "," << sector << "," << row << "); npads = " << npads << ", pitch = " << pitch 
	 << "\tprobablePad " << probablePad << endl;
  }
  return (probablePad);
}
//________________________________________________________________________________
Double_t StTpcCoordinateTransform::xFromPad(Int_t sector, Int_t row, Double_t pad) const {    // x coordinate in sector 12
  if (row > St_tpcPadConfigC::instance()->numberOfRows(sector)) row = St_tpcPadConfigC::instance()->numberOfRows(sector);
  Double_t pitch = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ?	
    St_tpcPadConfigC::instance()->innerSectorPadPitch(sector) : 
    St_tpcPadConfigC::instance()->outerSectorPadPitch(sector);
  Int_t npads = St_tpcPadConfigC::instance()->numberOfPadsAtRow(sector,row);
  Double_t xPad = -pitch*(pad - (npads+1.)/2.);
  if (_debug) {
    cout << "StTpcCoordinateTransform::xFromPad(" << sector << "," << row << "," << pad << "); npads = " << npads << ", pitch = " << pitch 
	 << "\txPad = " << xPad << endl;
  }
  Int_t NiRows = St_tpcPadConfigC::instance()->numberOfInnerRows(sector);
  if (NiRows == 13 || row > NiRows) {
    return xPad;
  }
  // iTPC Survey, Jim Thomas correction 08/21/18
  // The change in the yRef comes about because the origin of the coordinate system is 0.565 mm above the center of PadRow 40.   
  // The changes for the X coordinates come about because of the reversal of pad counting by the DAQ guys … as you know.  
  Double_t yRef = St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,NiRows) + 0.565; // Change sign in front of 0.565
  Double_t xL = xPad;  // Eliminate -1 in front of xPad
  Double_t yL = St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,row) - yRef;
  St_iTPCSurveyC *sur = St_iTPCSurveyC::instance();
  Double_t dx = sur->dx(sector-1);
//Double_t dy = sur->dy(sector-1);
  Double_t Xscale = sur->ScaleX(sector-1);
//Double_t Yscale = sur->ScaleY(sector-1);
  Double_t theta  = sur->Angle(sector-1);
  Double_t xHit = xL*(1. + Xscale) + dx - theta*yL; // Eliminate -1 in front of whole expression and delete ( )
//Double_t yHit = yL*(1. + Yscale) + dy + theta*xL + yRef;
  return xHit;
}
// Coordinate from Row
//
//Local Transformation...
//________________________________________________________________________________
Double_t StTpcCoordinateTransform::zFromTB(Double_t tb, Int_t sector, Int_t row, Int_t pad) const {
  if (row > St_tpcPadConfigC::instance()->numberOfRows(sector)) row = St_tpcPadConfigC::instance()->numberOfRows(sector);
  Double_t trigT0 = StTpcDb::instance()->triggerTimeOffset()*1e6;         // units are s
#if 0
  if ((sector <= 12 && tb <= 350) || // extra West laser off set, membrane cluster with time bucket > 350
      (sector >  12 && tb >  350)) {trigT0 +=  StTpcDb::instance()->triggerTimeOffsetWest()*1e6;}
#endif
  Double_t elecT0 = StTpcDb::instance()->Electronics()->tZero();          // units are us 
  Double_t sectT0 = St_tpcPadrowT0C::instance()->T0(sector,row);// units are us 
  Double_t t0 = trigT0 + elecT0 + sectT0;
  Int_t l = sector;
  if ( St_tpcPadConfigC::instance()->IsRowInner(sector,row)) l += 24;
  Double_t tbx = tb + St_tpcSectorT0offsetC::instance()->t0offset(l);
  if (St_tpcRDOT0offsetC::instance()->IsShfited(sector)) {
    tbx += St_tpcRDOT0offsetC::instance()->T0(sector,row,pad);
  }
  Double_t time = t0 + tbx*mTimeBinWidth; 
  Double_t z = StTpcDb::instance()->DriftVelocity(sector)*1e-6*time;
  return z;
}
//________________________________________________________________________________
Double_t StTpcCoordinateTransform::tBFromZ(Double_t z, Int_t sector, Int_t row, Int_t pad) const {
  if (row > St_tpcPadConfigC::instance()->numberOfRows(sector)) row = St_tpcPadConfigC::instance()->numberOfRows(sector);
  Double_t trigT0 = StTpcDb::instance()->triggerTimeOffset()*1e6;         // units are s
#if 0
  if ((sector <= 12 && z < 195) || // extra West laser off set, membrane cluster with time z < 195
      (sector >  12 && z > 195)) {trigT0 +=  StTpcDb::instance()->triggerTimeOffsetWest()*1e6;}
#endif
  Double_t elecT0 = StTpcDb::instance()->Electronics()->tZero();          // units are us 
  Double_t sectT0 = St_tpcPadrowT0C::instance()->T0(sector,row);// units are us 
  Double_t t0 = trigT0 + elecT0 + sectT0;
  Double_t time = z / (StTpcDb::instance()->DriftVelocity(sector)*1e-6);
  Int_t l = sector;
  if ( St_tpcPadConfigC::instance()->IsRowInner(sector,row)) l += 24;
  Double_t tb = (time - t0)/mTimeBinWidth - St_tpcSectorT0offsetC::instance()->t0offset(l);
  if (St_tpcRDOT0offsetC::instance()->IsShfited(sector)) tb -= St_tpcRDOT0offsetC::instance()->T0(sector,row,pad);
  return tb;
}
//________________________________________________________________________________
// FOR SECTOR 12 ONLY!!!! (Local coordinate);
Int_t StTpcCoordinateTransform::rowFromLocalY(Double_t y, Int_t sector) {
  static Int_t Nrows = 0;
  static Double_t *Radii = 0;
#ifndef __OLD__
  if (Nrows != St_tpcPadConfigC::instance()->padRows(sector)) {
    Nrows = St_tpcPadConfigC::instance()->padRows(sector);
    if (Radii) delete [] Radii;
    Radii = new Double_t[Nrows+1];
    for (Int_t i = 1; i <= Nrows+1; i++) {
      if (i == 1) {
	Radii[i-1] =  (3*St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,i) 
		       - St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,i+1))/2;
      } else if (i == Nrows + 1) {
	Radii[i-1] =  (3*St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,i-1) 
		       - St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,i-2))/2;
      } else {
	Radii[i-1] = (St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,i-1) +
		      St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,i))/2;
      }
    }
  }
  Long64_t row = TMath::BinarySearch(Nrows+1, Radii, y) + 1;
  if (row <= 0) row = 1;
  if (row > Nrows) row = Nrows;
  return row;
#else
  if (! Nrows) {
    Nrows = St_tpcPadPlanesC::instance()->padRows();
    Radii = new Double_t[Nrows];
    for (Int_t i = 1; i <= Nrows; i++) {
      Radii[i-1] = St_tpcPadPlanesC::instance()->radialDistanceAtRow(i);
    }
  }
  if (y < Radii[0]) return 1;
  if (y > Radii[Nrows-1]) return Nrows;
  Long64_t row = TMath::BinarySearch(Nrows, Radii, y);
  if (row < Nrows - 1) {
    if (TMath::Abs(Radii[row]-y) > TMath::Abs(Radii[row+1]-y)) row++;
  }
  row++;
  return row;
#endif
}
//________________________________________________________________________________
void  StTpcCoordinateTransform::operator()(const        StTpcLocalSectorCoordinate& a, StTpcLocalCoordinate& b           )
{ 
  StThreeVectorD xGG;
  Int_t row    = a.fromRow();
  Int_t sector = a.fromSector();
  if (row < 1 || row > St_tpcPadConfigC::instance()->numberOfRows(sector)) row    = rowFromLocal(a);
  StTpcDb::instance()->Pad2Tpc(a.sector(),row).LocalToMasterVect(a.position().xyz(),xGG.xyz()); 
  const Double_t *trans = StTpcDb::instance()->Pad2Tpc(sector,row).GetTranslation(); // 4
  TGeoTranslation GG2TPC(trans[0],trans[1],trans[2]);
  GG2TPC.LocalToMaster(xGG.xyz(), b.position().xyz());
  b.setSector(a.sector()); b.setRow(row);
}
//________________________________________________________________________________
void  StTpcCoordinateTransform::operator()(const              StTpcLocalCoordinate& a, StTpcLocalSectorCoordinate& b     ) 
{ 
  Int_t row    = a.fromRow();
  Int_t sector = a.fromSector();
  if ( ! (row >= 1 && row <= St_tpcPadConfigC::instance()->numberOfRows(sector))) {
    StThreeVectorD xyzS;
    StTpcDb::instance()->SupS2Tpc(sector).MasterToLocalVect(a.position().xyz(),xyzS.xyz());
    row = rowFromLocalY(xyzS[0], sector);
  }
  const Double_t *trans = StTpcDb::instance()->Pad2Tpc(a.sector(),row).GetTranslation(); // 4
  TGeoTranslation GG2TPC(trans[0],trans[1],trans[2]);
  StThreeVectorD xGG;
  GG2TPC.MasterToLocal(a.position().xyz(), xGG.xyz());
  StTpcDb::instance()->Pad2Tpc(a.sector(),row).MasterToLocalVect(xGG.xyz(),b.position().xyz()); b.setSector(a.sector()); b.setRow(row);
}
