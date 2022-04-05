// $Id: StFtpcConfMapPoint.cc,v 1.10 2007/01/15 08:23:01 jcs Exp $
// $Log: StFtpcConfMapPoint.cc,v $
// Revision 1.10  2007/01/15 08:23:01  jcs
// replace printf, cout and gMesMgr with Logger commands
//
// Revision 1.9  2004/04/06 18:36:12  oldi
// New data mebers for pad and time position and pad and time sigma added.
// Reference to StFtpcHit added.
// Possibility to update StFtpcHit coordinates directly included.
//
// Revision 1.8  2004/02/12 19:37:09  oldi
// *** empty log message ***
//
// Revision 1.7  2004/01/28 01:41:32  jeromel
// *** empty log message ***
//
// Revision 1.6  2003/01/20 13:16:22  oldi
// Additional volume segment added as garbage container. Hits which give a
// segment index which is out of range (esp. those ones sitting exactly on the
// beam line) are put in here.
// Handling of function GetSegm() simplified.
//
// Revision 1.5  2001/04/19 11:29:39  oldi
// Text of warning in SetAngles() changend.
//
// Revision 1.4  2000/11/10 18:34:31  oldi
// New constructor added.
//
// Revision 1.3  2000/07/18 21:22:15  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.2  2000/06/07 09:51:01  oldi
// Wrong setting in SetAllCoord(const StFtpcConfMapPoint *preceding_hit) in case
// of this == preceding_hit changed. Tracking with no vertex constraint works now
// poperly.
// Introduced a workaround for the exit(-1) in SetAngles().
//
// Revision 1.1  2000/05/11 15:14:38  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 13.07.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcConfMapPoint.hh"
#include "StMessMgr.h"

/////////////////////////////////////////////////////////////////////////////////////
//                                                                                 //
// StFtpcConfMapPoint class - representation of one cluster for the conformal      //
//                            mapping track algorithm.                             //
//                                                                                 //
// This class inherits all data members from StFtpcPoint which are the output      //
// of the FTPC cluster finder. Additionally it provides some data members and      //
// member functions which are necessary for the tracking algorithm.                //
// The errors are calulated using the Gaussian law of error propagation.           //
//                                                                                 //
/////////////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcConfMapPoint)


StFtpcConfMapPoint::StFtpcConfMapPoint() : StFtpcPoint()
{
  // Default constructor.
  // Sets additional values to zero.
  
  SetPhi(0.);
  SetEta(0.);
  
  SetXprime(0.);
  SetYprime(0.);
  SetXprimeerr(0.);
  SetYprimeerr(0.);
  SetIntPoint(0., 0., 0., 0., 0., 0.);
  SetShiftedCoord();
  SetDist(0., 0.);
}


StFtpcConfMapPoint::StFtpcConfMapPoint(StFtpcPoint* point, StFtpcVertex *vertex) : StFtpcPoint(*point)
{
  // "Copy" constructor.
  
  Setup(vertex);
}


StFtpcConfMapPoint::StFtpcConfMapPoint(Long_t   row, 
				       Long_t   sector, 
				       Long_t   n_pads, 
				       Long_t   n_bins, 
				       Long_t   max_adc, 
				       Long_t   charge,
				       Float_t  padpos,
				       Float_t  timepos,
				       Float_t  padpossigma,
				       Float_t  timepossigma,
				       Double_t x, 
				       Double_t y, 
				       Double_t z, 
				       Double_t x_err, 
				       Double_t y_err, 
				       Double_t z_err, 
				       Double_t s_phi, 
				       Double_t s_r, 
				       Long_t   flags) : StFtpcPoint(row, 
								     sector, 
								     n_pads, 
								     n_bins, 
								     max_adc, 
								     charge, 
								     padpos,
								     timepos,
								     padpossigma,
								     timepossigma,
								     x, 
								     y, 
								     z, 
								     x_err, 
								     y_err, 
								     z_err, 
								     s_phi, 
								     s_r, 
								     flags)
{
  // Constructor which takes its arguments directly from the cluster finder.
  // Sets additional values to zero.

  SetPhi(0.);
  SetEta(0.);
  
  SetXprime(0.);
  SetYprime(0.);
  SetXprimeerr(0.);
  SetYprimeerr(0.);
  SetIntPoint(0., 0., 0., 0., 0., 0.);
  SetShiftedCoord();
  SetDist(0., 0.);

  return;
}


StFtpcConfMapPoint::StFtpcConfMapPoint(Double_t *x, Int_t row, StFtpcVertex *vertex) : StFtpcPoint(x, row)
{
  // Constructor which handels arbitrary points with given coordinates.

  Setup(vertex);
}


StFtpcConfMapPoint::~StFtpcConfMapPoint() 
{
  // Destructor.
  // Does nothing except destruct.
}


void StFtpcConfMapPoint::Setup(StFtpcVertex *vertex)
{
  // Does the usual setup of a StFtpcConfMapPoint. It does it in the right order, especially.
  
  SetIntPoint(vertex->GetX(),    vertex->GetY(),    vertex->GetZ(), 
	      vertex->GetXerr(), vertex->GetYerr(), vertex->GetZerr());
  SetShiftedCoord();
  SetConfCoord();
  // The angles are set properly if they are set after the interaction point and the shifted coordinates
  SetAngles();
  SetDist(0., 0.);
  
  return;
}


void StFtpcConfMapPoint::SetIntPoint(const Double_t in_x,     const Double_t in_y,     const Double_t in_z,
				     const Double_t in_x_err, const Double_t in_y_err, const Double_t in_z_err)
{
  // Defines a new interaction point. This point is needed to calculate
  // the conformal coordinates. 
  
  SetXt(in_x);
  SetYt(in_y);
  SetZt(in_z);
  SetXterr(in_x_err);
  SetYterr(in_y_err);
  SetZterr(in_z_err);
  
  return;
}


void StFtpcConfMapPoint::SetAllCoord(const StFtpcConfMapPoint *preceding_hit)
{
  // Sets the interaction point, the shifted coordinates, and the conformal mapping coordinates.
  // These values are calculated from the interaction point of the given cluster which should be a
  // already found cluster on the same track.
  
  if (this == preceding_hit) {
    SetIntPoint(preceding_hit->GetX(),    preceding_hit->GetY(),    preceding_hit->GetZ(),
		preceding_hit->GetXerr(), preceding_hit->GetYerr(), preceding_hit->GetZerr());
  }
  
  else {  
    SetIntPoint(preceding_hit->GetXt(),    preceding_hit->GetYt(),    preceding_hit->GetZt(),
		preceding_hit->GetXterr(), preceding_hit->GetYterr(), preceding_hit->GetZterr());
  }
  
  SetShiftedCoord();
  SetConfCoord();

  return;
}


void StFtpcConfMapPoint::SetShiftedCoord()
{
  // Sets the coordinates with resepct to the given vertex point
  
  SetXv(GetX() - mXt);
  SetYv(GetY() - mYt);
  SetZv(GetZ() - mZt);
  SetXverr(TMath::Sqrt(GetXerr()*GetXerr() + mXterr*mXterr));
  SetYverr(TMath::Sqrt(GetYerr()*GetYerr() + mYterr*mYterr));
  SetZverr(TMath::Sqrt(GetZerr()*GetZerr() + mZterr*mZterr));
  
  return;
}


void StFtpcConfMapPoint::SetConfCoord() 
{
  // Calculates the conformal coordinates of one cluster.
  // If the option "vertex_constraint" applies the interaction point is 
  // assumed to be at (0, 0, 0). Otherwise the function will use the
  // interaction point specified by mXt and mYt.
  
  Double_t r2;
  
  if ((r2 = mXv*mXv + mYv*mYv)) {
    mXprime =  mXv / r2;
    mYprime = -mYv / r2;
    mXprimeerr = TMath::Sqrt(TMath::Power((-mXv * mXv +   mYv*mYv) * mXverr, 2) + TMath::Power( 2*mXv*mYv*mYverr, 2)) / TMath::Power(mXv*mXv + mYv*mYv, 2);
    mXprimeerr = TMath::Sqrt(TMath::Power((-mXv * mXv - 3*mYv*mYv) * mYverr, 2) + TMath::Power(-2*mXv*mYv*mXverr, 2)) / TMath::Power(mXv*mXv + mYv*mYv, 2);
  }
  
  else {
    mXprime    = 0.;
    mYprime    = 0.;
    mXprimeerr = 0.;
    mYprimeerr = 0.;
  }
  
  return;
}


void StFtpcConfMapPoint::SetAngles()
{
  // Calculates the angle phi and the pseudorapidity eta for each cluster.
  // So to say this is just a transformation of the coordinate system.
  
  Double_t r3dim = TMath::Sqrt(mXv*mXv + mYv*mYv + mZv*mZv);
  Double_t r2dim = TMath::Sqrt(mXv*mXv + mYv*mYv);

  if (r2dim == 0.) {
    // If r2dim == 0 the pseudorapidity eta cannot be calculated (division by zero)!
    // This can only happen if the point is lying on the z-axis and this should never be possible.
    LOG_WARN << "The pseudorapidity cannot be calculated! (2-dim radius is zero and set to 1.e-10.)" << endm;
    r2dim = 1.e-10;
  }
  
  if (mXv == 0.) {
    mPhi = (mYv > 0.) ? TMath::Pi() / 2. : - TMath::Pi() / 2.;
  }

  else {
    mPhi = (mXv > 0.) ? TMath::ASin(mYv/r2dim) : TMath::Pi() - TMath::ASin(mYv/r2dim);
  }

  if (mPhi < 0.) {
    mPhi += 2. * TMath::Pi();
  }

  mEta = 0.5 * TMath::Log((r3dim + mZv)/(r3dim - mZv));

  return;
}
