//StiHit.cxx
//M.L. Miller (Yale Software)
//04/01
#include <Stiostream.h>
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
						<<"LOCAL :"<<hit.x()<<" "<<hit.y()<<" "<<hit.z()<< endl
						<<"GLOBAL:"<<hit.x_g()<<" "<<hit.y_g()<<" "<<hit.z_g()<< endl
						<<hit.sxx()<<" "<<hit.syy()<<" "<<hit.szz()<<"\t" //Diagonal Error
						<<hit.sxy()<<" "<<hit.sxz()<<" "<<hit.syz()<<"\t" //Off-diagonal error
						<<hit.detector() //pointer to detector
						<<"times Used: "<<hit.timesUsed();
}


double StiHit::getValue(int key) const
{
  double value;
  switch (key)
    {
    case kZ: value = _zg; break;
    case kR: value = ::sqrt(_xg*_xg+_yg*_yg); break;
    case kPseudoRapidity: value = getPseudoRapidity(); break;
    case kPhi: value = atan2(_yg,_xg);break;
    default: value = -999999.; break;
    }
  return value;  
}
  
double StiHit::getPseudoRapidity() const
{
  double r=::sqrt(_xg*_xg+_yg*_yg);
  double tanTheta = ::tan(::atan2(r,_zg)/2.);
  if (tanTheta>0.)
    return -::log(tanTheta);
  else
    return 1.e10;
}


void StiHit::setGlobal(const StiDetector * detector,
			      const StMeasuredPoint * stHit,
			      float gx, float gy, float gz,
			      float energy)
{
  if (detector)
    {
      StiPlacement * placement = detector->getPlacement();
      mrefangle = placement->getNormalRefAngle();
      mposition = placement->getLayerRadius();
      mx =  detector->_cos*gx + detector->_sin*gy;
      my = -detector->_sin*gx + detector->_cos*gy - placement->getNormalYoffset();
      //my = -detector->_sin*gx + detector->_cos*gy + placement->getNormalYoffset();
    }
  else
    {
      mrefangle = 0.;
      mposition = 0.; 
      mx =  gx;
      my =  gy;
    }
  mz = gz;
  msxx = 1.;//sxx;
  msyy = 1.;//syy;
  mszz = 1.;//szz;
  msxy = 0.;//sxy;
  msxz = 0.;//sxz;
  msyz = 0.;//syz;  
  _xg = gx;
  _yg = gy;
  _zg = gz;
  mTimesUsed = 0;
  mdetector = detector;  msthit = stHit;
  _energy = energy;
}


 void StiHit::set(const StiDetector * detector,
			const StMeasuredPoint * stHit,
			float energy,
			float x, float y, float z, 
			float sxx, float sxy, float sxz, float syy, float syz, float szz)
{
  if (detector)
    {
      StiPlacement * placement = detector->getPlacement();
      mrefangle = placement->getNormalRefAngle();
      mposition = placement->getLayerRadius();
      _xg = detector->_cos*x - detector->_sin*y;
      _yg = detector->_sin*x + detector->_cos*y;
    }
  else
    {
      mrefangle = 0.;
      mposition = 0.;
      _xg = x;
      _yg = y;
    }
  mx = x;
  my = y;
  mz = z;
  msxx = sxx;
  msyy = syy;
  mszz = szz;
  msxy = sxy;
  msxz = sxz;
  msyz = syz;  
  mTimesUsed = 0;
  mdetector = detector;
  msthit = stHit;
  _energy = energy;  
  _zg = z;
}

 void StiHit::setTimesUsed(unsigned int val)
{
    mTimesUsed=val;
}

 float StiHit::getEloss()
{
  return _energy;
}

 const StThreeVectorF StiHit::globalPosition() const
{
  return StThreeVectorF(_xg,_yg,_zg); ////msthit->position();
}

void StiHit::set(float position,  float angle, float y, float z)
{
  mrefangle = angle;
  mposition = position;
  mx = position;
  my = y;
  mz = z;
  msxx = msyy = mszz = msxy = msxz = msyz = 0.;
  mTimesUsed=0;
  mdetector = 0;
  msthit = 0;
  _energy = 0;
}
