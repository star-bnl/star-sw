// $Id: StFtpcPoint.cc,v 1.17 2004/01/28 01:41:32 jeromel Exp $
// $Log: StFtpcPoint.cc,v $
// Revision 1.17  2004/01/28 01:41:32  jeromel
// *** empty log message ***
//
// Revision 1.16  2003/09/23 23:25:35  oldi
// SetGlobalCoord() and SetUsage() moved to the end of the constructors to avoid
// them calling GetFlags() before mFlags is initialized.
//
// Revision 1.15  2003/09/16 15:27:02  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.14  2003/01/16 18:04:33  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.13  2002/11/21 15:46:21  oldi
// Enabled rotation for FTPC west. If there is an observed shift of the vertex
// position in y-direction (in FTPC west), just fill this offset into the Db.
// Up to now this offset is set to 0., i.e. only FTPC east is rotated (because
// the offset is at 0.3427 cm).
//
// Revision 1.12  2002/11/19 12:45:07  oldi
// A new database entry (installationPointY[east/west]) was introduced. Now
// the rotation of FTPC east is done around the correct axis, which isn't
// measured but comes from the drawings. The measurements used before were true
// measurements but had nothing to do with the rotation axis, unfortunately.
// Anyway, the difference is rather small since a typical cluster is rotated
// by less than 0.1mm.
// Some code cleanup done.
//
// Revision 1.11  2002/10/31 13:39:09  oldi
// InstallationPointZ() changed to InstallationPointZ(i) where i specifies FTPC east or west.
//
// Revision 1.10  2002/10/11 15:45:12  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
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

  SetResidualsToZero();

  SetGlobalCoord(kFALSE);
  SetUsage(kFALSE);

  return;
}


StFtpcPoint::StFtpcPoint(fcl_fppoint_st *point_st)
{
  // Standard constructor.
  // This is the usual way to create a StFtpcPoint object. By giving the pointer
  // to the fcl_fppoint_st(ructure) (cluster data found by the cluster finder) the 
  // constructor copies the cluster information into its data members.

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

  SetResidualsToZero();

  SetGlobalCoord(GetGlobalFlag());
  SetUsage(kFALSE);

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

  SetResidualsToZero();

  SetGlobalCoord(GetGlobalFlag());
  SetUsage(kFALSE);

  return;
}  


StFtpcPoint::StFtpcPoint(Double_t *x, Int_t row)
{
  // Constructor which takes the x, y, and z coodrinate and the pad row.

  // Hit position is set in local coordinates per default! Change flag (SetGlobalCoord(kTRUE)) if necessary.

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

  SetResidualsToZero();

  SetGlobalCoord(kFALSE); // Set to local per default.
  SetUsage(kFALSE);

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
   
    StThreeVectorD org(mCoord.X(), mCoord.Y(), mCoord.Z());

    // internal FTPC rotation
    Int_t i = (org.z() < 0) ? 0 : 1; // east or west

    // first tranformation to new origin (FTPC installation point)
    org.setY(org.y() - StFtpcTrackingParams::Instance()->InstallationPointY(i));
    org.setZ(org.z() - StFtpcTrackingParams::Instance()->InstallationPointZ(i));
    
    // actual rotation
    org = StFtpcTrackingParams::Instance()->FtpcRotation(i) * org;
    
    // set z-position back to original value
    org.setY(org.y() + StFtpcTrackingParams::Instance()->InstallationPointY(i));      
    org.setZ(org.z() + StFtpcTrackingParams::Instance()->InstallationPointZ(i));
    
    StThreeVectorD transform = StFtpcTrackingParams::Instance()->TpcToGlobalRotation() * org + StFtpcTrackingParams::Instance()->TpcPositionInGlobal();
    
    mCoord.SetX(transform.x());
    mCoord.SetY(transform.y());
    mCoord.SetZ(transform.z());
    
    SetGlobalCoord(kTRUE);
  }
  
  else {
    // hit is in global coordinates already
    gMessMgr->Message("", "W", "OS") << "Hit is in global coordinates already! Not transformed." << endm;
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
    
    StThreeVectorD org(mCoord.X(), mCoord.Y(), mCoord.Z());
    StThreeVectorD transform = StFtpcTrackingParams::Instance()->GlobalToTpcRotation() * (org - StFtpcTrackingParams::Instance()->TpcPositionInGlobal());

    // internal FTPC rotation
    Int_t i = (transform.z() < 0) ? 0 : 1; // east or west
    
    // first tranformation to new origin (FTPC installation point)
    transform.setY(transform.y() - StFtpcTrackingParams::Instance()->InstallationPointY(i));
    transform.setZ(transform.z() - StFtpcTrackingParams::Instance()->InstallationPointZ(i));
    
    // actual rotation
    transform = StFtpcTrackingParams::Instance()->FtpcRotationInverse(i) * transform;
    
    // set z-position back to original value
    transform.setY(transform.y() + StFtpcTrackingParams::Instance()->InstallationPointY(i));
    transform.setZ(transform.z() + StFtpcTrackingParams::Instance()->InstallationPointZ(i));
    
    mCoord.SetX(transform.x());
    mCoord.SetY(transform.y());
    mCoord.SetZ(transform.z());
    
    SetGlobalCoord(kFALSE);
  }
  
  else {
    // hit is in local (FTPC) coordinates already
    gMessMgr->Message("", "W", "OS") << "Hit is in local (FTPC) coordinates already! Not transformed." << endm;
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

void StFtpcPoint::SetResidualsToZero()
{
  // Sets all residuals to 0.

  SetXPrimResidual(0.);
  SetYPrimResidual(0.);
  SetRPrimResidual(0.);
  SetPhiPrimResidual(0.);
  SetXGlobResidual(0.);
  SetYGlobResidual(0.);
  SetRGlobResidual(0.);
  SetPhiGlobResidual(0.);

  return;
}


StFtpcTrack *StFtpcPoint::GetTrack(TObjArray *tracks) const
{
  // Returns the pointer to the track to which this hit belongs.

  return (StFtpcTrack*)tracks->At(this->GetTrackNumber());
}


void StFtpcPoint::SetTrackedFlag(Bool_t tracked) 
{
  // Sets flag, if the cluster was used for tracking.
  // This has to be done due to consistency with the point bank.

  Long_t old_flag = GetFlags();
  SetFlags((old_flag & 0xFFFFFFDF) | ((Long_t)tracked*32));
}


Bool_t StFtpcPoint::GetTrackedFlag()
{
  // Returns true, if 'tracked flag' is set, otherwise returns false.

  return (Bool_t)(GetFlags() & (Long_t)32);
}


void StFtpcPoint::SetGlobalFlag(Bool_t global) 
{
  // Sets flag, if the cluster is measured in global coordinates.

  Long_t old_flag = GetFlags();
  SetFlags((old_flag & 0xFFFFFFAF) | ((Long_t)global*64));
}


Bool_t StFtpcPoint::GetGlobalFlag()
{
  // Returns true, if 'global flag' is set, otherwise returns false.

  return (Bool_t)(GetFlags() & (Long_t)64);
}


void StFtpcPoint::SetUnusableForTrackingFlag(Bool_t global) 
{
  // Sets flag, if the cluster DOES NOT pass all quality criteria to be used for tracking.

  Long_t old_flag = GetFlags();
  SetFlags((old_flag & 0xFFFFFFAF) | ((Long_t)global*128));
}


Bool_t StFtpcPoint::GetUnusableForTrackingFlag()
{
  // Returns true, if 'UnusableForTracking flag' is set, otherwise returns false.

  return (Bool_t)(GetFlags() & (Long_t)128);
}


Bool_t StFtpcPoint::IsUsable()
{
  // Returns true, if 'UsableForTracking flag' and 'tracked flag' is set, otherwise returns false.
    
  return (Bool_t)((!GetTrackedFlag()) && !(GetUnusableForTrackingFlag()));
}


Bool_t StFtpcPoint::IsUnusable()
{
  // Returns true, if 'UsableForTracking flag' and 'tracked flag' is set, otherwise returns false.
    
  return (Bool_t)(GetTrackedFlag() || GetUnusableForTrackingFlag());
}
