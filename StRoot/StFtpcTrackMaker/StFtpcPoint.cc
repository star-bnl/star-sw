// $Id: StFtpcPoint.cc,v 1.5 2000/08/01 12:23:15 hummler Exp $
// $Log: StFtpcPoint.cc,v $
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
//----------Last Modified: 18.07.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcPoint.hh"

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

  SetUsage(false);
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

  SetUsage(false);
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

  return;
}


StFtpcPoint::StFtpcPoint(Double_t *x, Int_t row)
{
  // Constructor which takes the x, y, and z coodrinate and the pad row.

  SetUsage(false);
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
  SetFlags(0.);

  return;
}


StFtpcPoint::~StFtpcPoint() 
{
  // Destructor.
  // Does nothing except destruct.
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

Int_t StFtpcPoint::Write()
{
  // Writes cluster to disc.
  // Does nothing up to now.

  return 0;
}
