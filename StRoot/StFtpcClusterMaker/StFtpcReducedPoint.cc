// $Id: StFtpcReducedPoint.cc,v 1.1 2000/11/24 15:02:34 hummler Exp $
// $Log: StFtpcReducedPoint.cc,v $
// Revision 1.1  2000/11/24 15:02:34  hummler
// commit changes omitted in last commit
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 19.09.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcReducedPoint.hh"

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// StFtpcReducedPoint class - representation of one cluster for the FTPC trackers. //
//                                                                          //
// This class contains all data members which are the output of the FTPC    //
// cluster finder.                                                          //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcReducedPoint)


StFtpcReducedPoint::StFtpcReducedPoint()
{
  // Default constructor.
  // Sets all pointers to zero.

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



StFtpcReducedPoint::~StFtpcReducedPoint() 
{
  // Destructor.
  // Does nothing except destruct.
}


Int_t StFtpcReducedPoint::ToTable(fcl_fppoint_st *point_st)
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


