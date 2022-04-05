//StiHit.cxx
//M.L. Miller (Yale Software)
//04/01
#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include <Stiostream.h>
#include "StEventTypes.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiShape.h"
#include "StiUtilities/StiDebug.h"


//_____________________________________________________________________________
StiHit::StiHit()
{
   reset();
}


//_____________________________________________________________________________
StiHit::StiHit(const StiHit &h) 
{
  memcpy(mBeg,h.mBeg,mEnd-mBeg+1);
}

//_____________________________________________________________________________
const StiHit& StiHit::operator=(const StiHit & h)
{
  memcpy(mBeg,h.mBeg,mEnd-mBeg+1);
  return *this;
}

//_____________________________________________________________________________
StiHit::~StiHit()
{}

//_____________________________________________________________________________
/// Convenience method to perform a rotation
/// along the z axis
void StiHit::rotate(double alpha)
{
  assert(!mdetector);
static float rotMx[3][3]={{1,0,0},{0,1,0},{0,0,1}}, s[6];

  mrefangle+=alpha;
  double ca = cos(alpha);
  double sa = sin(alpha);
  rotMx[0][0] = ca; rotMx[0][1] = sa;
  rotMx[1][0] =-sa; rotMx[1][1] = ca;

  double x = rotMx[0][0]*mx + rotMx[0][1]*my;
  double y = rotMx[1][0]*mx + rotMx[1][1]*my;
  mx = x; my = y;

// 		S=R*S*Rt
  memcpy(s,&msxx,sizeof(s));
  TCL::trasat(rotMx[0],s,&msxx,3,3);
}

//_____________________________________________________________________________
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
//_____________________________________________________________________________
void StiHit::setError(const float matrix[6])
{
  memcpy(&msxx,matrix,6*sizeof(msxx));
}  

//_____________________________________________________________________________
/*! Streamer for StiHit objects. */
ostream& operator<<(ostream& os, const StiHit& hit)
{
  return os <<hit.refangle() <<" "<<hit.position()
	    <<"L:"<<hit.x()  <<" "<<hit.y()  <<" "<<hit.z()
	    <<"G:"<<hit.x_g()<<" "<<hit.y_g()<<" "<<hit.z_g();
}


//_____________________________________________________________________________
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
  
//_____________________________________________________________________________
double StiHit::getPseudoRapidity() const
{
  double r=::sqrt(_xg*_xg+_yg*_yg);
  double tanTheta = ::tan(::atan2(r,_zg)/2.);
  if (tanTheta>0.)
    return -::log(tanTheta);
  else
    return 1.e10;
}
//_____________________________________________________________________________
void StiHit::reset()
{
  memset(mBeg,0,mEnd-mBeg+1);
  mMaxTimes = 1;
static unsigned int myCount=0;  
  mCount = ++myCount;
}


//_____________________________________________________________________________
void StiHit::setGlobal(const StiDetector * detector,
		       const StMeasuredPoint * stHit,
		       float gx, float gy, float gz,
		       float energy)
{
static int nCall =0; nCall++;

  if (detector)
    {
      StiPlacement * placement = detector->getPlacement();
      mrefangle = placement->getLayerAngle();
      mposition = placement->getLayerRadius();
      mx =  detector->_cos*gx + detector->_sin*gy;
      my = -detector->_sin*gx + detector->_cos*gy;
      mz = gz;
      double dd[3]={mx,my,mz};


      if (!detector->insideL(dd,7,1.2)) {
         LOG_ERROR <<
           Form("**** StiHit.%s outside (%d) by %g (%g) ****"
           ,detector->getName().c_str()
	   ,StiDetector::mgIndex
	   ,StiDetector::mgValue[0],StiDetector::mgValue[1])
         << endm;
          assert( detector->insideL(dd,7,1.5));     // 
      }

      mx =  detector->_cos*gx + detector->_sin*gy;
      my = -detector->_sin*gx + detector->_cos*gy;
    }
  else
    {
      mrefangle = 0.;
      mposition = 0.; 
      mx =  gx;
      my =  gy;
      mz = gz;
    }
  msxx = -1.;//sxx;
  msyy = -1.;//syy;
  mszz = -1.;//szz;
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


//_____________________________________________________________________________
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
  LOG_ERROR <<

  Form("**** StiHit.%s too far: x=%f pos=%g dif=%g ****\n"
        ,detector->getName().c_str(),mx,pos,dif)
        << endm;
}

//_____________________________________________________________________________
 void StiHit::addTimesUsed()
{
    mTimesUsed++;
    assert(mTimesUsed <=mMaxTimes);
}
//_____________________________________________________________________________
 void StiHit::subTimesUsed()
{
    mTimesUsed--;
    assert(mTimesUsed>=0);
}

//_____________________________________________________________________________
 float StiHit::getEloss()
{
  return _energy;
}

//_____________________________________________________________________________
 const StThreeVectorF StiHit::globalPosition() const
{
  return StThreeVectorF(_xg,_yg,_zg); ////msthit->position();
}

//_____________________________________________________________________________
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
//_____________________________________________________________________________
void StiHit::makeDca()
{
  memset(mBeg,0,mEnd-mBeg+1);
  mszz = 1e12;
}
//_____________________________________________________________________________
int StiHit::isDca() const
{
  if (mdetector) 	return 0;
  if (mx || my || mz)	return 0;
  if (mszz<1000)	return 0;
  return 1;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include "StEvent/StHit.h"
//_____________________________________________________________________________
int StiHit::idTruth() const
{
  StHit *stHit = (StHit*)msthit;
  if (!stHit) return 0;
  return stHit->idTruth();
}

//_____________________________________________________________________________
int StiHit::qaTruth() const
{
  StHit *stHit = (StHit*)msthit;
  if (!stHit) return 0;
  return stHit->qaTruth();
}




