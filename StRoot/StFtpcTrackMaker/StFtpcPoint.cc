// $Id: StFtpcPoint.cc,v 1.1 2000/05/11 15:14:47 oldi Exp $
// $Log: StFtpcPoint.cc,v $
// Revision 1.1  2000/05/11 15:14:47  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 11.05.2000
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
}


StFtpcPoint::~StFtpcPoint() 
{
  // Destructor.
  // Does nothing except destruct.
}


StFtpcTrack *StFtpcPoint::GetTrack(TClonesArray *tracks) const
{
  // Returns the pointer to the track to which this hit belongs.

  return (StFtpcTrack*)tracks->At(this->GetTrackNumber());
}


Int_t StFtpcPoint::Write()
{
  // Writes cluster to disc.
  // Does nothing up to now.

  return 0;
}
