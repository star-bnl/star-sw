//StiHit.cxx
//M.L. Miller (Yale Software)
//04/01
#include <iostream.h>
#include "StEventTypes.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"

StiHit::StiHit()
:  mrefangle(0),
   mposition(0),
   mx(0),
   my(0),
   mz(0), 
   msxx(0),
   msyy(0),
   mszz(0),
   msxy(0),
   msxz(0),
   msyz(0),
   _xg(0),
   _yg(0),
   _zg(0),
   mTimesUsed(0),
   mdetector(0),
   msthit(0),
   _energy(0)
{
  reset();
}


StiHit::StiHit(const StiHit & h) 
:  mrefangle(h.mrefangle),
   mposition(h.mposition),
   mx(h.mx),
   my(h.my),
   mz(h.mz), 
   msxx(h.msxx),
   msyy(h.msyy),
   mszz(h.mszz),
   msxy(h.msxy),
   msxz(h.msxz),
   msyz(h.msyz),
   _xg(0),
   _yg(0),
   _zg(0),
   mTimesUsed(h.mTimesUsed),
   mdetector(h.mdetector),
   msthit(h.msthit),
   _energy(h._energy)
{
}

const StiHit& StiHit::operator=(const StiHit & h)
{
	mrefangle = h.mrefangle;
	mposition = h.mposition;
	mx = h.mx;
	my = h.my;
	mz = h.mz; 
	msxx = h.msxx;
	msyy = h.msyy;
	mszz = h.mszz;
	msxy = h.msxy;
	msxz = h.msxz;
	msyz = h.msyz; 
	_xg  = h._xg; 
	_yg  = h._yg;
	_zg  = h._zg;
	mTimesUsed = h.mTimesUsed;
	mdetector = h.mdetector;
	msthit = h.msthit;
	_energy = h._energy;
	return *this;
}

StiHit::~StiHit()
{}

/// Convenience method to perform a rotation
/// along the z axis
void StiHit::rotate(double alpha)
{
  double ca = cos(alpha);
  double sa = sin(alpha);
  double rxx = ca; double rxy = sa;
  double ryx =-sa; double ryy = ca;
  double x = rxx*mx + rxy*my;
  double y = ryx*mx + ryy*my;
  mx = x;
  my = y;
  // A=R*S
  double axx = rxx*msxx + rxy*msxy;
  double axy = rxx*msxy + rxy*msyy;
  double ayx = ryx*msxx + ryy*msxy;
  //double ayy = ryx*msxy + ryy*msyy;
  // S=A*Rt
  msxx = axx*rxx + axy*ryx;
  msxy = axx*ryx + axy*ryy;
  //msyx = ayx*rxx + ayy*rxy;
  msyy = ayx*ryx + axy*ryy;
}

void StiHit::setError(const StMatrixF& matrix)
{
  enum Labels {x=1, y=2, z=3};
  
  //Set Diagonal elements
  msxx = matrix(x,x);
  msyy = matrix(y,y);
  mszz = matrix(z,z);
  //Off Diagonal
  msxy = matrix(x,y);
  msxz = matrix(x,z);
  msyz = matrix(y,z);
  return;
}


/*! Streamer for StiHit objects. */
ostream& operator<<(ostream& os, const StiHit& hit)
{
  return os <<hit.refangle()<<" "<<hit.position()<<"\t\t" //Key
	    <<hit.x()<<" "<<hit.y()<<" "<<hit.z()<<"\t\t" //Position
	    <<hit.sxx()<<" "<<hit.syy()<<" "<<hit.szz()<<"\t" //Diagonal Error
	    <<hit.sxy()<<" "<<hit.sxz()<<" "<<hit.syz()<<"\t" //Off-diagonal error
	    <<hit.detector() //pointer to detector
	    <<"times Used: "<<hit.timesUsed();
}


