/***********************************************************************
 *
 * $Id: StTpcCoordinateTransform.cc,v 1.33 2009/03/16 14:23:59 fisyak Exp $
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
#include "StMessMgr.h"
#include "StDetectorDbMaker/St_tpcSectorT0offsetC.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "TMath.h"
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using namespace units;
#endif
// StTpcCoordinateTransform::StTpcCoordinateTransform(StTpcGeometry* geomdb,
// 						   StTpcSlowControl* scdb,
// 						   StTpcElectronics* eldb)
//ClassImp(StTpcCoordinateTransform);
static Int_t _debug = 0;
StTpcCoordinateTransform::StTpcCoordinateTransform(StTpcDb* globalDbPointer)
 {
  
//     mTPCdb = geomdb;
//     mSCdb  = scdb;
//     mElectronicsDb = eldb;
    gTpcDbPtr    = globalDbPointer;
    if (gTpcDbPtr->PadPlaneGeometry() &&
	gTpcDbPtr->Electronics() &&
	gTpcDbPtr->SlowControlSim() && 
        gTpcDbPtr->GlobalPosition()) { 
	mTimeBinWidth = 1./gTpcDbPtr->Electronics()->samplingFrequency();
	//
	// For this version I'll put the inner/outer sector z offsets by
	// hand, since StTpcDb doesn't have them.  I'll take them out when
	// it does.
	//	mInnerSectorzOffset = .35;
	//	mOuterSectorzOffset = 0.;
	//	mDriftDistance = 210.1;
	//DH 24-Feb-2000 -- these parameters are now available
        //define the outer drift distance as z = 0 in the tpc local
        //coordinate system.
        mDriftDistance = gTpcDbPtr->Dimensions()->gatingGridZ();
        mInnerSectorzOffset = gTpcDbPtr->Dimensions()->zInnerOffset();
        mOuterSectorzOffset = gTpcDbPtr->Dimensions()->zOuterOffset();
	Double_t beta = 0;
	Int_t numSectors = gTpcDbPtr->Dimensions()->numberOfSectors();
	Double_t deg_2_rad = M_PI/180.;
	for (Int_t sector = 1; sector <= numSectors;
	     sector++) {
	    beta = (sector>12) ? (numSectors-sector)*2.*M_PI/(static_cast<double>(numSectors)/2.)
		: sector*2.*M_PI/(static_cast<double>(numSectors)/2.);
// 	    if (sector==12) sector=0;
// 	    if (sector<12) {
// 		sector=15-sector;
// 	    }
	    mCosForSector[sector-1] = cos(beta); // careful, sector is the sector number, not index
	    mSinForSector[sector-1] = sin(beta); // careful, sector is the sector number, not index
	    Double_t s = -1;
	    if (sector > 12) s = +1;
	    mInnerPositionOffsetX[sector-1] = s*gTpcDbPtr->SectorPosition()->innerPositionOffsetX(sector-1);
	    mOuterPositionOffsetX[sector-1] = s*gTpcDbPtr->SectorPosition()->outerPositionOffsetX(sector-1);
	    mInnerRotation[sector-1]        = s*gTpcDbPtr->SectorPosition()->innerRotation(sector-1)*deg_2_rad;
	    mOuterRotation[sector-1]        = s*gTpcDbPtr->SectorPosition()->outerRotation(sector-1)*deg_2_rad;
// 	    PR(beta);
// 	    PR(cos(beta));
// 	    PR(sin(beta));
	    
	}
    }
    else {
	gMessMgr->Error() << "StTpcDb IS INCOMPLETE! Cannot contstruct Coordinate transformation." << endm;
	assert(gTpcDbPtr->PadPlaneGeometry());
	assert(gTpcDbPtr->Electronics());
	assert(gTpcDbPtr->SlowControlSim()); 
        assert(gTpcDbPtr->GlobalPosition());
    }
}
StTpcCoordinateTransform::~StTpcCoordinateTransform() { /* nopt */ }

//      Raw Data          <-->  Global Coordinate
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a, StGlobalCoordinate& b, Bool_t useT0, Bool_t useTau)
{// useT0 = kTRUE for pad and kFALSE for cluster, useTau = kTRUE for data cluster and  = kFALSE for MC
    StTpcLocalCoordinate tmp2;
    StTpcLocalSectorCoordinate tmp1;
    this->operator()(a,tmp1, useT0, useTau);
    this->operator()(tmp1,tmp2); 
    this->operator()(tmp2,b);
}

void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a, StTpcPadCoordinate& b, Int_t sector, Int_t row, Bool_t useT0, Bool_t useTau)
{ // useT0 = kTRUE for pad and kFALSE for cluster, useTau = kTRUE for data cluster and  = kFALSE for MC
    StTpcLocalCoordinate tmp1;
    StTpcLocalSectorCoordinate tmp2;
    this->operator()(a,tmp1,sector,row);
    this->operator()(tmp1,tmp2); 
    this->operator()(tmp2,b, useT0, useTau);
}
//      Raw Data          <-->  TPC Local  Coordinate
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a, StTpcLocalCoordinate& b, Bool_t useT0, Bool_t useTau)
{ // useT0 = kTRUE for pad and kFALSE for cluster, useTau = kTRUE for data cluster and  = kFALSE for MC
    
    StTpcLocalSectorCoordinate tmp;
    this->operator()(a,tmp, useT0, useTau);
  
    this->operator()(tmp,b);
}

void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StTpcPadCoordinate& b, Bool_t useT0, Bool_t useTau)
{ // useT0 = kTRUE for pad and kFALSE for cluster, useTau = kTRUE for data cluster and  = kFALSE for MC

   
    StTpcLocalSectorCoordinate tmp;
    this->operator()(a,tmp);
    
    this->operator()(tmp,b, useT0, useTau);
}


//________________________________________________________________________________
//      Local Sector Coordnate    <->  Tpc Raw Pad Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate& a, StTpcPadCoordinate& b, Bool_t useT0, Bool_t useTau)
{ // useT0 = kTRUE for pad and kFALSE for cluster, useTau = kTRUE for data cluster and  = kFALSE for MC
  Int_t sector = a.fromSector();
  Int_t row    = a.fromRow();
  if (row < 1 || row > 45) row    = rowFromLocal(a);
    
  Float_t probablePad = padFromLocal(a);
  Double_t zoffset=(row>13) ?
    mOuterSectorzOffset
    :mInnerSectorzOffset;
  Double_t t0offset = (useT0 && sector>=1&&sector<=24) ? gTpcDbPtr->tpcT0()->T0(sector, row, TMath::Nint (probablePad)) : 0;
  t0offset *= mTimeBinWidth;
  if (! useT0 && useTau) // for cluster
    t0offset -= 3.0 * St_tss_tssparC::instance()->tau();   // correct for convolution lagtime
  Double_t t0zoffset = t0offset*gTpcDbPtr->DriftVelocity(sector)*1e-6;
  Float_t tb = tBFromZ(a.position().z()+zoffset-t0zoffset,sector);  
  b = StTpcPadCoordinate(sector, row, probablePad, tb);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcPadCoordinate& a,  StTpcLocalSectorCoordinate& b, Bool_t useT0, Bool_t useTau) 
{ // useT0 = kTRUE for pad and kFALSE for cluster, useTau = kTRUE for data cluster and = kFALSE for MC
  StThreeVector<double>  tmp=xyFromRaw(a);
  Double_t zoffset= (a.row()>13) ? mOuterSectorzOffset : mInnerSectorzOffset;
  Double_t t0offset = useT0 ? gTpcDbPtr->tpcT0()->T0(a.sector(),a.row(),TMath::Nint(a.pad())) : 0;
  t0offset *= mTimeBinWidth;
  if (! useT0 && useTau) // for cluster
    t0offset -= 3.0 * St_tss_tssparC::instance()->tau();   // correct for convolution lagtime
  Double_t t0zoffset = t0offset*gTpcDbPtr->DriftVelocity(a.sector())*1e-6;
  //t0 offset -- DH  27-Mar-00
  Double_t z = zFromTB(a.timeBucket(),a.sector())-zoffset+t0zoffset;
  tmp.setZ(z);
  b = StTpcLocalSectorCoordinate(tmp,a.sector(),a.row());
}
//  Tpc Local Sector <--> Global
 
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate& a, StGlobalCoordinate& b)
{
     StTpcLocalCoordinate tmp;
     this->operator()(a,tmp);
     this->operator()(tmp,b); 
    
}
void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a,  StTpcLocalSectorCoordinate& b, Int_t sector, Int_t row)
{
    StTpcLocalCoordinate tmp;
    this->operator()(a,tmp, sector,row);
    this->operator()(tmp,b); 
}
void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a,  StTpcLocalSectorAlignedCoordinate& b, Int_t sector, Int_t row)
{
    StTpcLocalCoordinate tmp;
    this->operator()(a,tmp, sector,row);
    this->operator()(tmp,b); 
}
//      Local Sector Coordnate    -->  Tpc Local Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate& a, StTpcLocalCoordinate& b)
{
   
  Int_t sector = a.fromSector();
  Int_t row    = a.fromRow();  


    StThreeVector<double> sector12Position(a.position().x(),
					   a.position().y() ,
					   a.position().z());
    StThreeVector<double> tmp = rotateToLocal(sector12Position,sector);

    b = StTpcLocalCoordinate(tmp,sector,row);
}
//      Local Sector Coordnate    -->  Tpc Local Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorAlignedCoordinate& a, StTpcLocalCoordinate& b)
{
   
  Int_t sector = a.fromSector();
  Int_t row    = a.fromRow();  


    StThreeVector<double> sector12Position(a.position().x(),
					   a.position().y() ,
					   a.position().z());
    StThreeVector<double> tmp = rotateToLocal(sector12Position,sector);

    b = StTpcLocalCoordinate(tmp,sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate& a, 
					  StTpcLocalSectorAlignedCoordinate& b) {
  Int_t sector = a.fromSector(); 
  Int_t row    = a.fromRow();
  if (row < 1 || row > 45) row    = rowFromLocal(a);
  Double_t offset = row <14 ? mInnerPositionOffsetX[sector-1] : mOuterPositionOffsetX[sector-1];
  Double_t xx  = a.position().x() - offset;
  Double_t yy  = a.position().y() - 123;
  Double_t rot = row <14 ? mInnerRotation[sector-1] : mOuterRotation[sector-1];
  b = StTpcLocalSectorAlignedCoordinate(StThreeVector<double>(xx*cos(rot) + yy*sin(rot),
						       -xx*sin(rot) + yy*cos(rot) + 123,
						       a.position().z()),sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorAlignedCoordinate& a, 
					  StTpcLocalSectorCoordinate& b) {
  Int_t sector = a.fromSector(); 
  Int_t row    = a.fromRow();
  if (row < 1 || row > 45) row    = rowFromLocal(a);
  
  Double_t xx  = a.position().x();
  Double_t yy  = a.position().y() - 123;
  Double_t offset = row <14 ? mInnerPositionOffsetX[sector-1] : mOuterPositionOffsetX[sector-1];
  Double_t rot = row <14 ? mInnerRotation[sector-1] : mOuterRotation[sector-1];
  b = StTpcLocalSectorCoordinate(StThreeVector<double>(xx*cos(rot) - yy*sin(rot) + offset,
						       xx*sin(rot) + yy*cos(rot) + 123,
						       a.position().z()),sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StTpcLocalSectorCoordinate& b)
{
  Int_t sector = a.fromSector(); 
  if (sector < 1 || sector > 24) sector= sectorFromCoordinate(a);
  StThreeVector<double> tmp=rotateFromLocal(a.position(),sector);
    
  b = StTpcLocalSectorCoordinate(tmp,sector,a.fromRow());
  Int_t row    = b.fromRow();
  if (row < 1 || row > 45) {
    row    = rowFromLocal(b);
    b  = StTpcLocalSectorCoordinate(b.position(),sector,row);
  }
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StTpcLocalSectorAlignedCoordinate& b)
{
  Int_t sector = a.fromSector(); 
  if (sector < 1 || sector > 24) sector= sectorFromCoordinate(a);
  StThreeVector<double> tmp=rotateFromLocal(a.position(),sector);
  b = StTpcLocalSectorAlignedCoordinate(tmp,sector,a.fromRow());
  Int_t row    = b.fromRow();
  if (row < 1 || row > 45) {
    row    = rowFromLocal(b);
    b  = StTpcLocalSectorAlignedCoordinate(b.position(),sector,row);
  }
}
//      Local Coordinate  <--> Global Coordinate
void StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate& a, StGlobalCoordinate& b) 
{
    // Requires survey DB i/o!
    // Use matrix rotations and offset from database.  Hardtke, 22-may-2001
    Double_t xyzL[3] = {a.position().x(),a.position().y(),a.position().z()};
    Double_t xyzG[3];
    gTpcDbPtr->Tpc2GlobalMatrix().LocalToMaster(xyzL,xyzG);
    b = StGlobalCoordinate(xyzG[0],xyzG[1],xyzG[2]);
}

void StTpcCoordinateTransform::operator()(const StGlobalCoordinate& a, StTpcLocalCoordinate& b, Int_t sector, Int_t row)
{
    // Requires survey DB i/o!
    // Use matrix rotations and offset from database.  Hardtke, 22-may-2001
    Double_t xyzG[3] = {a.position().x(),a.position().y(),a.position().z()};
    Double_t xyzL[3];
    gTpcDbPtr->Tpc2GlobalMatrix().MasterToLocal(xyzG,xyzL);
    b = StTpcLocalCoordinate(xyzL[0],xyzL[1],xyzL[2],sector,row);
    if (sector < 1 || sector > 24) {
      sector= sectorFromCoordinate(b);
      b = StTpcLocalCoordinate(b.position(),sector,row);
    }
}

StThreeVector<double> StTpcCoordinateTransform::sector12Coordinate(StThreeVector<double>& v, Int_t *sector)
{
    *sector = sectorFromCoordinate(v);
    return  rotateFromLocal(v,*sector);
}

StThreeVector<double>
StTpcCoordinateTransform::padCentroid(StTpcLocalSectorCoordinate& localSector, Int_t *pad, Int_t *row)
{
    StTpcLocalSectorCoordinate centerOfPad;
    Int_t nRow    = localSector.fromRow();
    if (nRow < 1 || nRow > 45) nRow    = rowFromLocal(localSector);
    StTpcPadCoordinate tmp(12,                      //sector
			   nRow,     //row
			   padFromLocal(localSector), // pad
			   localSector.fromSector());
    
    this->operator()(tmp,centerOfPad);
    *row = nRow;
    return centerOfPad.position();
}
/***********************************************************************/
/*                       TRANSFORMATION ROUTINES                       */

// sector from Tpc local coordinates
Int_t StTpcCoordinateTransform::sectorFromCoordinate(const StThreeVector<double>& a) const
{
    // 30 degrees should be from db

    Double_t angle = atan2(a.y(),a.x());
    if(angle<0) angle+= 2*M_PI;
//     Int_t sectorNumber= (int)( (angle+4*M_PI/3.)/(2*M_PI/3.));
    Int_t sectorNumber= (int)( (angle+2*M_PI/24.)/(2*M_PI/12.));
    if(a.z()>0){
               sectorNumber=15-sectorNumber;
               if(sectorNumber>12)sectorNumber-=12;
               }
    else
              {
               sectorNumber+=9;
               if(sectorNumber<=12)sectorNumber+=12;
               }

     return(sectorNumber);
}

// FOR SECTOR 12 ONLY!!!! (Local coordinate);
Int_t StTpcCoordinateTransform::rowFromLocal(const StThreeVector<double>& b) const
{
    Double_t referencePosition;
    Double_t rowPitch;
    Int_t    offset;
    Double_t boundary =
	gTpcDbPtr->PadPlaneGeometry()->outerSectorEdge();
    if(b.y() > boundary) {    // in the outer sector
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
    Int_t probableRow =
	static_cast<int>( (b.y() - (referencePosition-rowPitch/2))/rowPitch )+offset;

    if(b.y() < boundary && probableRow>13) {
	probableRow=13;
    } 
    if(b.y() > boundary && probableRow<14){ 
	probableRow=14;
    }
    if (probableRow<1)
	probableRow = 1;
    if (probableRow>45)
	probableRow=45;
    
//     PR(probableRow);

    return (probableRow);
}


Float_t StTpcCoordinateTransform::padFromLocal(const StThreeVector<double>& b, Int_t row) const
{
  return padFromX(b.x(), row);
}
Float_t StTpcCoordinateTransform::padFromX(Double_t x, Int_t row) const
{
  Double_t pitch = (row<14) ?
    gTpcDbPtr->PadPlaneGeometry()->innerSectorPadPitch() :
    gTpcDbPtr->PadPlaneGeometry()->outerSectorPadPitch();
  // x coordinate in sector 12
  Float_t probablePad = (gTpcDbPtr->PadPlaneGeometry()->numberOfPadsAtRow(row)+1.)/2. - x/pitch;
  // CAUTION: pad cannot be <1
    if(probablePad<0.500001) {
// 	gMessMgr->Error() << "ERROR in pad From Local.\n";
// 	gMessMgr->Error() << "Pad is calculated to be '" << probablePad << "'\n";
// 	gMessMgr->Error() << "Assigning Pad='1'"<< endm;
      probablePad=0.500001;
    }
    return (probablePad);
  
    //    return (int) (-x/pitch
    //		  + ((double)(gTpcDbPtr->PadPlaneGeometry()->numberOfPadsAtRow(row)) + 1.)/2.);
}

//
// Coordinate from Raw
//
StThreeVector<double> StTpcCoordinateTransform::xyFromRaw(const StTpcPadCoordinate& a)
{
    Double_t localY = yFromRow(a.row());
    Double_t localX = xFromPad(a.row(),a.pad());
    
    return(StThreeVector<double>(localX,localY,0));
}

//Local Transformation...
Double_t StTpcCoordinateTransform::yFromRow(Int_t row)  const
{
    // Returns y coordinate in sector 12
    return (gTpcDbPtr->PadPlaneGeometry()->radialDistanceAtRow(row));
}


Double_t StTpcCoordinateTransform::xFromPad(Int_t row, Float_t pad) const
{
    // x coordinate in sector 12
    Double_t pitch = (row<14) ?
	gTpcDbPtr->PadPlaneGeometry()->innerSectorPadPitch() :
	gTpcDbPtr->PadPlaneGeometry()->outerSectorPadPitch();
    Double_t x = -pitch*(pad - (gTpcDbPtr->PadPlaneGeometry()->numberOfPadsAtRow(row)+1.)/2.);
    return x;
}

Double_t StTpcCoordinateTransform::zFromTB(Float_t tb, Int_t sector) const
{
    Double_t timeBin = tb; // to avoid using const_cast<int> & static_cast<double>
    //    Double_t z = 
    //      gTpcDbPtr->DriftVelocity()*1e-6*         //cm/s->cm/us
      //        (-gTpcDbPtr->Electronics()->tZero() + (timeBin+.5)*mTimeBinWidth);  // z= tpc local sector  z,no inner outer offset yet.
       Double_t z = 
         gTpcDbPtr->DriftVelocity(sector)*1e-6*                //units are cm/s->cm/us
	 (gTpcDbPtr->triggerTimeOffset()*1e6                   // units are s
	  + gTpcDbPtr->Electronics()->tZero()                  // units are us 
	  + (timeBin + St_tpcSectorT0offsetC::instance()->t0offset(sector))*(mTimeBinWidth));  // 
   
    return(z);
}

Float_t StTpcCoordinateTransform::tBFromZ(Double_t z, Int_t sector) const
{
    //PR(gTpcDbPtr->PadPlaneGeometry->driftDistance()); // Not available yet.
    //PR(z);
  // z is in tpc local sector coordinate system. z>=0;
   
   
    Double_t time = (
		     -1*(gTpcDbPtr->triggerTimeOffset()*1e6  // units are s
			 + gTpcDbPtr->Electronics()->tZero()
			 )   // units are us 
		     + ( z / (gTpcDbPtr->DriftVelocity(sector)*1e-6))
		     ); // tZero + (z/v_drift); the z already has the proper offset
    
    return ( (Float_t)(time/mTimeBinWidth) - St_tpcSectorT0offsetC::instance()->t0offset(sector) );
}

//
// Rotation Matrices
//

StThreeVector<double>
StTpcCoordinateTransform::rotateToLocal(const StThreeVector<double>& a,
				     Int_t sector, Int_t idir)
{   //  to local means " from sector 12 local to tpc local"

    // Speed up the code, don't use matrices.
    // Use array of cosines and sines created during construction.
    // careful, sector is the sector number, not index
    // rotation is in opposite sense than the "fromLocal" rotation, only change
    // sign of sin(b) because sin is odd and cos is even.
    
    // ( cos Þ  sin Þ )
    // ( -sin Þ   cos Þ )
  // idir != 1 means transform direction
    Double_t x = (sector>12)?a.x():-a.x(); // Undo the sign flip before rotation
    
    StThreeVector<double> result(x*mCosForSector[sector-1] + a.y()*mSinForSector[sector-1],
			  -x*mSinForSector[sector-1] + a.y()*mCosForSector[sector-1],
			  0.); // z is done in the next line

    if (idir == 1) {
//       return (sector>12)? (StThreeVector<double>(result.x(),result.y(),a.z()-mDriftDistance))
// 	: (StThreeVector<double>(result.x(),result.y(),-a.z()+mDriftDistance));
      Double_t z = mDriftDistance-a.z();
      if (sector > 12) z = -z;
      return StThreeVector<double>(result.x(),result.y(),z);
    }
    else 
      return (sector>12)? (StThreeVector<double>(result.x(),result.y(),a.z()))
	: (StThreeVector<double>(result.x(),result.y(),-a.z()));
}
 
StThreeVector<double> 
StTpcCoordinateTransform::rotateFromLocal(const StThreeVector<double>& a,
					  Int_t sector, Int_t idir)
{   
    // FromLocal means " from the tpc local to sector 12 local"
    // define 2x2 rotation matrix
    //
    // ( cos Þ  -sin Þ )
    // ( sin Þ   cos Þ )

    // Speed up the code, don't use matrices.
    // Use array of cosines and sines created during construction.
    // careful, sector is the sector number, not index
  // idir != 1 means transform direction
    StThreeVector<double> result(a.x()*mCosForSector[sector-1] - a.y()*mSinForSector[sector-1],
			  a.x()*mSinForSector[sector-1] + a.y()*mCosForSector[sector-1],
			  0.); // z is done in the next line
    //     PR(result);
    if (idir == 1) {
      //      return (sector>12) ? (StThreeVector<double>(result.x(),result.y(),a.z()+mDriftDistance))
      //	: (StThreeVector<double>(-result.x(),result.y(),-a.z()+mDriftDistance));
      if (sector > 12) {
	Double_t z = a.z()+mDriftDistance; 
	return StThreeVector<double>(result.x(),result.y(),z);
      }
      else {
	Double_t z = a.z()-mDriftDistance;
	return StThreeVector<double>(-result.x(),result.y(),-z);
      }
    }
    else
      return (sector>12) ? (StThreeVector<double>(result.x(),result.y(),a.z()))
	               : (StThreeVector<double>(-result.x(),result.y(),-a.z()));
}

/****************************************************************/
/*                         UTILITIES                            */
Double_t StTpcCoordinateTransform::rad2deg(Double_t a) const
{
    return(57.2957795*a);
}

Int_t StTpcCoordinateTransform::nearestInteger(Double_t a) const
{
    return static_cast<int>(a);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalDirection& a, StTpcLocalSectorDirection& b) {   
  Int_t sector= a.fromSector();
  Int_t row   = a.fromRow();  
  StThreeVector<double> tmp=rotateFromLocal(a.position(),sector,2);
  b = StTpcLocalSectorDirection(tmp,sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalDirection& a, StTpcLocalSectorAlignedDirection& b) {   
  Int_t sector= a.fromSector();
  Int_t row   = a.fromRow();  
  StThreeVector<double> tmp=rotateFromLocal(a.position(),sector,2);
  b = StTpcLocalSectorAlignedDirection(tmp,sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorDirection& a, StGlobalDirection& b) {
  StTpcLocalDirection tmp;
  this->operator()(a,tmp);
  this->operator()(tmp,b); 
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StGlobalDirection& a,  StTpcLocalSectorDirection& b,Int_t sector, Int_t row) {
  StTpcLocalDirection tmp;
  this->operator()(a,tmp,sector,row);
  this->operator()(tmp,b); 
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StGlobalDirection& a,  StTpcLocalSectorAlignedDirection& b,Int_t sector, Int_t row) {
  StTpcLocalDirection tmp;
  this->operator()(a,tmp,sector,row);
  this->operator()(tmp,b); 
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorDirection& a, StTpcLocalDirection& b) {
  Int_t sector = a.fromSector();
  Int_t row    = a.fromRow();
  StThreeVector<double> sector12Position(a.position().x(),
					 a.position().y() ,
					 a.position().z());
  StThreeVector<double> tmp = rotateToLocal(sector12Position,sector,2);
  b = StTpcLocalDirection(tmp,sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorAlignedDirection& a, StTpcLocalDirection& b) {
  Int_t sector = a.fromSector();
  Int_t row    = a.fromRow();
  StThreeVector<double> sector12Position(a.position().x(),
					 a.position().y() ,
					 a.position().z());
  StThreeVector<double> tmp = rotateToLocal(sector12Position,sector,2);
  b = StTpcLocalDirection(tmp,sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorDirection& a, 
					  StTpcLocalSectorAlignedDirection& b) {
  Int_t sector = a.fromSector(); 
  Int_t row    = a.fromRow();
  Double_t xx  = a.position().x();
  Double_t yy  = a.position().y();
  Double_t rot = row <14 ? mInnerRotation[sector-1] : mOuterRotation[sector-1];
  b = StTpcLocalSectorAlignedDirection(StThreeVector<double>(xx*cos(rot) + yy*sin(rot),
							     -xx*sin(rot) + yy*cos(rot),
							     a.position().z()),sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalSectorAlignedDirection& a, 
					  StTpcLocalSectorDirection& b) {
  Int_t sector = a.fromSector(); 
  Int_t row    = a.fromRow();
  Double_t xx  = a.position().x();
  Double_t yy  = a.position().y();
  Double_t rot = row <14 ? mInnerRotation[sector-1] : mOuterRotation[sector-1];
  b = StTpcLocalSectorDirection(StThreeVector<double>(xx*cos(rot) - yy*sin(rot),
					       xx*sin(rot) + yy*cos(rot),
					       a.position().z()),sector,row);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StTpcLocalDirection& a, StGlobalDirection& b) 
{
    // Requires survey DB i/o!
    // Use matrix rotations and offset from database.  Hardtke, 22-may-2001
    Double_t dirL[3] = {a.position().x(),a.position().y(),a.position().z()};
    Double_t dirG[3];
    gTpcDbPtr->Tpc2GlobalMatrix().LocalToMasterVect(dirL,dirG);
    b = StGlobalDirection(dirG[0],dirG[1],dirG[2]);
}
//________________________________________________________________________________
void StTpcCoordinateTransform::operator()(const StGlobalDirection& a, StTpcLocalDirection& b, Int_t sector, Int_t row)
{
  // Requires survey DB i/o!
  // Use matrix rotations and offset from database.  Hardtke, 22-may-2001
    Double_t dirG[3] = {a.position().x(),a.position().y(),a.position().z()};
    Double_t dirL[3];
    gTpcDbPtr->Tpc2GlobalMatrix().MasterToLocalVect(dirG,dirL);
    b = StTpcLocalDirection(dirL[0],dirL[1],dirL[2],sector,row);
}
//________________________________________________________________________________
