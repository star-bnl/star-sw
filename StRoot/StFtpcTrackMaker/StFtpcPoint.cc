// $Id: StFtpcPoint.cc,v 1.9 2002/06/04 13:34:57 oldi Exp $
// $Log: StFtpcPoint.cc,v $
// Revision 1.9  2002/06/04 13:34:57  oldi
// Transformation of local FTPC coordinates in global coordinates introduced.
// An additional flag keeps track in which coordinate system the point position
// is measured.
// 'Tracked' flag is set correctly, now.
//
// Revision 1.8  2002/03/05 16:53:10  jcs
// force data type definitions to avoid compiler warnings (this is a correct
// but inelegant fix which must be changed)
//
// Revision 1.7  2002/01/29 11:07:59  oldi
// Write() renamed to WriteCluster() resp. WriteTrack() to avoid compiler warnings.
// As a result the functions TObject::Write() are available again (directly).
//
// Revision 1.6  2000/11/10 18:37:46  oldi
// New constructor added.
// StThreeVector replaced by TVector3 to be able to use ROOT output (e.g. Write()).
// Cleanup.
//
// Revision 1.5  2000/08/01 12:23:15  hummler
// add writing to table functionality
//
// Revision 1.4  2000/07/18 21:22:16  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.3  2000/06/13 14:49:01  oldi
// Added SetTrackedFlag(Bool_t tracked) and GetTrackedFlag() to take care of the
// bit 5 of mFlags.
// Changed SetUsage(Bool_t f) to change the bit 5 of mFlags, too.
//
// Revision 1.2  2000/06/07 11:37:34  oldi
// Cleanup.
//
// Revision 1.1  2000/05/11 15:14:47  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 19.09.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcPoint.hh"
#include "StMessMgr.h"

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// StFtpcPoint class - representation of one cluster for the FTPC trackers. //
//                                                                          //
// This class contains all data members which are the output of the FTPC    //
// cluster finder.                                                          //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcPoint)


StFtpcPoint::StFtpcPoint()
{
  // Default constructor.
  // Sets all pointers to zero.
  
  SetGlobalCoord(kFALSE);

  SetUsage(kFALSE);
  SetHitNumber(-1);
  SetNextHitNumber(-1);
  SetTrackNumber(-1);

  SetPadRow(0);
  SetSector(0);

  SetNumberPads(0);
  SetNumberBins(0);

  SetMaxADC(0);
  SetCharge(0);

  SetX(0);
  SetY(0);
  SetZ(0);

  SetXerr(0);
  SetYerr(0);
  SetZerr(0);

  SetSigmaPhi(0);
  SetSigmaR(0);
  SetFlags(0);

  return;
}


StFtpcPoint::StFtpcPoint(fcl_fppoint_st *point_st)
{
  // Standard constructor.
  // This is the usual way to create a StFtpcPoint object. By giving the pointer
  // to the fcl_fppoint_st(ructure) (cluster data found by the cluster finder) the 
  // constructor copies the cluster information into its data members.

  SetUsage(kFALSE);
  SetHitNumber(-1);
  SetNextHitNumber(-1);
  SetTrackNumber(-1);

  SetPadRow((Long_t) point_st->row);
  SetSector((Long_t) point_st->sector);

  SetNumberPads((Long_t) point_st->n_pads);
  SetNumberBins((Long_t) point_st->n_bins);

  SetMaxADC((Long_t) point_st->max_adc);
  SetCharge((Long_t) point_st->charge);

  SetX((Double_t) point_st->x);
  SetY((Double_t) point_st->y);
  SetZ((Double_t) point_st->z);

  SetXerr((Double_t) point_st->x_err);
  SetYerr((Double_t) point_st->y_err);
  SetZerr((Double_t) point_st->z_err);

  SetSigmaPhi((Double_t) point_st->s_phi);
  SetSigmaR((Double_t) point_st->s_r);
  SetFlags((Long_t) point_st->flags);

  SetGlobalCoord(GetGlobalFlag());

  return;
}


StFtpcPoint::StFtpcPoint(Long_t   row, 
			 Long_t   sector, 
			 Long_t   n_pads, 
			 Long_t   n_bins, 
			 Long_t   max_adc, 
			 Long_t   charge, 
			 Double_t x, 
			 Double_t y, 
			 Double_t z, 
			 Double_t x_err, 
			 Double_t y_err, 
			 Double_t z_err, 
			 Double_t s_phi, 
			 Double_t s_r, 
			 Long_t   flags)
{
  // Constructor which fills all values found by the cluster finder directly.

  SetUsage(kFALSE);
  SetHitNumber(-1);
  SetNextHitNumber(-1);
  SetTrackNumber(-1);

  SetPadRow(row);
  SetSector(sector);

  SetNumberPads(n_pads);
  SetNumberBins(n_bins);

  SetMaxADC(max_adc);
  SetCharge(charge);

  SetX(x);
  SetY(y);
  SetZ(z);

  SetXerr(x_err);
  SetYerr(y_err);
  SetZerr(z_err);

  SetSigmaPhi(s_phi);
  SetSigmaR(s_r);
  SetFlags(flags);

  SetGlobalCoord(GetGlobalFlag());

  return;
}  


StFtpcPoint::StFtpcPoint(Double_t *x, Int_t row)
{
  // Constructor which takes the x, y, and z coodrinate and the pad row.

  // Hit position is set in local coordinates per default! Change flag (SetGlobalCoord(kTRUE)) if necessary.
  SetGlobalCoord(kFALSE); // Set to local per default.

  SetUsage(kFALSE);
  SetHitNumber(-1);
  SetNextHitNumber(-1);
  SetTrackNumber(-1);

  SetPadRow(row);
  SetSector(-1);

  SetNumberPads(-1);
  SetNumberBins(-1);

  SetMaxADC(-1);
  SetCharge(0);

  SetX(x[0]);
  SetY(x[1]);
  SetZ(x[2]);

  SetXerr(0.);
  SetYerr(0.);
  SetZerr(0.);

  SetSigmaPhi(0.);
  SetSigmaR(0.);
  SetFlags(0);

  return;
}


StFtpcPoint::~StFtpcPoint() 
{
  // Destructor.
  // Does nothing except destruct.
}


void StFtpcPoint::TransformFtpc2Global()
{
  // Coordinate Transformation.
  // Shift and Rotation of Ftpc coordinates due to the rotation of the TPC with respect to the magnet.
  // (The FTPC was aligned with respect to the TPC!)
  // Turns FTPC due to observed shift of reconstructed vertex position in y direction.
  // Errors are left as they were.

  if (!IsInGlobalCoord()) {
   
    StFtpcTrackingParams *params = StFtpcTrackingParams::Instance();
    
    StThreeVectorD org(mCoord.X(), mCoord.Y(), mCoord.Z());

    // internal FTPC rotation (FTPC east only)
    if (org.z() < 0) {
      // check if hit is in FTPC east
      
      // first tranformation to new origin (FTPC installation point)
      org.setZ(org.z() - params->InstallationPointZ());
      
      // actual rotation
      org = params->FtpcRotation() * org;
      
      // set z-position back to original value
      org.setZ(org.z() + params->InstallationPointZ());
    }
    
    StThreeVectorD transform = params->TpcToGlobalRotation() * org + params->TpcPositionInGlobal();
    
    mCoord.SetX(transform.x());
    mCoord.SetY(transform.y());
    mCoord.SetZ(transform.z());
    
    SetGlobalCoord(kTRUE);
  }
  
  else {
    // hit is in global coordinates already
    gMessMgr->Message("", "W", "OST") << "Hit is in global coordinates already! Not transformed." << endm;
  }
  
  return;
}


void StFtpcPoint::TransformGlobal2Ftpc()
{
  // Coordinate Transformation.
  // Shift and Rotation of Ftpc coordinates due to the rotation of the TPC with respect to the magnet.
  // (The FTPC was aligned with respect to the TPC!)
  // Turns FTPC due to observed shift of reconstructed vertex position in y direction.
  // Errors are left as they were.
  
  if (IsInGlobalCoord()) {
    
    StFtpcTrackingParams *params = StFtpcTrackingParams::Instance();
    
    StThreeVectorD org(mCoord.X(), mCoord.Y(), mCoord.Z());
    StThreeVectorD transform = params->GlobalToTpcRotation() * (org - params->TpcPositionInGlobal());

    // internal FTPC rotation (FTPC east only)
    if (transform.z() < 0) {
      // check if hit is in FTPC east
      
      // first tranformation to new origin (FTPC installation point)
      transform.setZ(transform.z() - params->InstallationPointZ());
      
      // actual rotation
      transform = params->FtpcRotationInverse() * transform;
      
      // set z-position back to original value
      transform.setZ(transform.z() + params->InstallationPointZ());
    }
    
    mCoord.SetX(transform.x());
    mCoord.SetY(transform.y());
    mCoord.SetZ(transform.z());
    
    SetGlobalCoord(kFALSE);
  }

  else {
    // hit is in local (FTPC) coordinates already
    gMessMgr->Message("", "W", "OST") << "Hit is in local (FTPC) coordinates already! Not transformed." << endm;
  }

  return;
}


Int_t StFtpcPoint::ToTable(fcl_fppoint_st *point_st)
{
  point_st->row=GetPadRow();
  point_st->sector=GetSector();
  point_st->n_pads=GetNumberPads();
  point_st->n_bins=GetNumberBins();
  point_st->max_adc=GetMaxADC();
  point_st->charge=GetCharge();
  point_st->flags=GetFlags();
  point_st->x=GetX();
  point_st->y=GetY();
  point_st->z=GetZ();
  point_st->x_err=GetXerr();
  point_st->y_err=GetYerr();
  point_st->z_err=GetZerr();
  point_st->s_phi=GetSigmaPhi();
  point_st->s_r=GetSigmaR();

  return 1;
}


Int_t StFtpcPoint::WriteCluster()
{
  // Writes cluster to disc.
  // Does nothing up to now.

  return 0;
}
