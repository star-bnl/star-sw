// $Id: StFtpcConfMapHit.cc,v 1.1 2000/05/10 13:39:07 oldi Exp $
// $Log: StFtpcConfMapHit.cc,v $
// Revision 1.1  2000/05/10 13:39:07  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 20.04.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcConfMapHit.hh"

/////////////////////////////////////////////////////////////////////////////////////
//                                                                                 //
// StFtpcConfMapHit class - representation of one cluster for the conformal        //
//                          mapping track algorithm.                               //
//                                                                                 //
// This class inherits all data members from StFtpcHit which are the output        //
// of the FTPC cluster finder. Additionally it provides some data members and      //
// member functions which are necessary for the tracking algorithm.                //
// The errors are calulated using the Gaussian law of error propagation.           //
//                                                                                 //
/////////////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcConfMapHit)


StFtpcConfMapHit::StFtpcConfMapHit() : StFtpcHit()
{
  // Default constructor.
  // Sets additioanl values to zero.
  
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


StFtpcConfMapHit::StFtpcConfMapHit(fcl_fppoint_st *point_st, StFtpcVertex *vertex) : StFtpcHit(point_st)
{
  // Standard constructor.
  // This is the usual way to create a StFtpcConfMapHit object. By giving the pointer
  // to the fcl_fppoint_st(ructure) the constructor copies the pointers to the cluster information
  // into its data members and calculates necessary information for the confromal mapping 
  // track algorithm. 
  
  Setup(vertex);
}


StFtpcConfMapHit::~StFtpcConfMapHit() 
{
  // Destructor.
  // Does nothing except destruct.
}


void StFtpcConfMapHit::Setup(StFtpcVertex *vertex)
{
  // Does the usual setup of a StFtpcConfMapHit. It does it in the right order, especially.
  
  SetIntPoint(vertex->GetX(),    vertex->GetY(),    vertex->GetZ(), 
	      vertex->GetXerr(), vertex->GetYerr(), vertex->GetZerr());
  SetShiftedCoord();
  SetConfCoord();
  // The angles are set properly if they are set after the interaction point and the shifted coordinates
  SetAngles();
  SetDist(0., 0.);
  
  return;
}


void StFtpcConfMapHit::SetIntPoint(const Double_t in_x,     const Double_t in_y,     const Double_t in_z,
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


void StFtpcConfMapHit::SetAllCoord(const StFtpcConfMapHit *preceding_hit)
{
  // Sets the interaction point, the shifted coordinates, and the conformal mapping coordinates.
  // These values are calculated from the interaction point of the given cluster which should be a
  // already found cluster on the same track.
  
  if (this == preceding_hit) {
    SetIntPoint(preceding_hit->GetXv(),    preceding_hit->GetYv(),    preceding_hit->GetZv(),
		preceding_hit->GetXverr(), preceding_hit->GetYverr(), preceding_hit->GetZverr());
  }
  
  else {  
    SetIntPoint(preceding_hit->GetXt(),    preceding_hit->GetYt(),    preceding_hit->GetZt(),
		preceding_hit->GetXterr(), preceding_hit->GetYterr(), preceding_hit->GetZterr());
  }
  
  SetShiftedCoord();
  SetConfCoord();
  
  return;
}


void StFtpcConfMapHit::SetShiftedCoord()
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


void StFtpcConfMapHit::SetConfCoord() 
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


void StFtpcConfMapHit::SetAngles()
{
  // Calculates the angle phi and the pseudorapidity eta for each cluster.
  // So to say this is just a transformation of the coordinate system.
  
  Double_t r2dim = TMath::Sqrt(mXv*mXv + mYv*mYv);
  Double_t r3dim = TMath::Sqrt(mXv*mXv + mYv*mYv + mZv*mZv);
  
  if (r2dim) {
    
    if (mXv == 0.) 
      mPhi = (mYv > 0.) ? TMath::Pi() / 2. : - TMath::Pi() / 2.;
    
    else 
      mPhi = (mXv > 0.) ? TMath::ASin(mYv/r2dim) : TMath::Pi() - TMath::ASin(mYv/r2dim);
    
    if (mPhi < 0.) 
      mPhi += 2. * TMath::Pi();
    
    mEta = 0.5 * TMath::Log((r3dim + mZv)/(r3dim - mZv));
  }
  
  else {
    cout << "The pseudorapidity eta cannot be calculated (division by zero)!" << endl;
    exit(-1);
  }
  
  return;
}
