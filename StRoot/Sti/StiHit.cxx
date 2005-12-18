//StiHit.cxx
//M.L. Miller (Yale Software)
//04/01
#include <Stiostream.h>
#include "StEventTypes.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "TSystem.h"
#include "TRandom.h"
#ifdef Sti_DEBUG
#include "TRMatrix.h"
#include "TRVector.h"
#define PrP(A)    cout << "\t" << (#A) << " = \t" << ( A )
#define PrPP(A,B) cout << "=== StiHit::" << (#A); PrP((B)); cout << endl;
#endif


StiHit::StiHit()
{
   reset();
}


StiHit::StiHit(const StiHit &h) 
{
  memcpy(mBeg,h.mBeg,mEnd-mBeg+1);
}

const StiHit& StiHit::operator=(const StiHit & h)
{
  memcpy(mBeg,h.mBeg,mEnd-mBeg+1);
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
  double ayy = ryx*msxy + ryy*msyy;
  // S=A*Rt
  msxx = axx*rxx + axy*rxy;
  msxy = axx*ryx + axy*ryy;
  msyy = ayx*ryx + ayy*ryy;
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
  return os <<hit.refangle() <<" "<<hit.position()
	    <<"L:"<<hit.x()  <<" "<<hit.y()  <<" "<<hit.z()
	    <<"G:"<<hit.x_g()<<" "<<hit.y_g()<<" "<<hit.z_g();
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
void StiHit::reset()
{
  memset(mBeg,0,mEnd-mBeg+1);
static unsigned int myCount=0;  
  mCount = ++myCount;
}


void StiHit::setGlobal(const StiDetector * detector,
			      const StMeasuredPoint * stHit,
			      float gx, float gy, float gz,
			      float energy)
{
  if (detector)
    {
      StiPlacement * placement = detector->getPlacement();
      mrefangle = placement->getLayerAngle();
      mposition = placement->getLayerRadius();
      mx =  detector->_cos*gx + detector->_sin*gy;
      my = -detector->_sin*gx + detector->_cos*gy;
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
  if (!stHit   ) return;
  if (!detector) return;
  double pos = detector->getPlacement()->getNormalRadius();
  double dif = mx-pos;
  if (fabs(dif)<2.) return;
  printf("**** StiHit.%s too far: x=%f pos=%g dif=%g ****\n"
        ,detector->getName().c_str(),mx,pos,dif);
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
      mrefangle = placement->getLayerAngle();
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
  if (!stHit) return;
  assert( fabs(stHit->position().x()-_xg)< 1.e-6
       && fabs(stHit->position().y()-_yg)< 1.e-6
       && fabs(stHit->position().z()-_zg)< 1.e-6);

  if (!detector) return;
  double pos = detector->getPlacement()->getNormalRadius();
  double dif = mx-pos;
  if (fabs(dif)<1.) return;
  printf("**** StiHit.%s too far: x=%f pos=%g dif=%g ****\n"
        ,detector->getName().c_str(),mx,pos,dif);
}

 void StiHit::setTimesUsed(unsigned int val)
{
    mTimesUsed=(unsigned char)val;
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
  memset(mBeg,0,mEnd-mBeg+1);
  mrefangle = angle;
  mposition = position;
  mx = position;
  my = y;
  mz = z;
//   add crazy values
  _xg = 100000.;
  _yg = 100000.;
  _zg = 100000.;
}
