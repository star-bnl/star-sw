//StvHit.cxx
//M.L. Miller (Yale Software)
//04/01
//Rewritten V.Perev 01/2010
#include <stdlib.h>
#include <assert.h>
#include "TError.h"
#include "TCernLib.h"
#include "Stiostream.h"
#include "StvHit.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"


//_____________________________________________________________________________
StvHit::StvHit()
{
   reset();
}

//_____________________________________________________________________________
StvHit::StvHit(const StvHit &h) 
{
  memcpy(mBeg,h.mBeg,mEnd-mBeg+1);
}

//_____________________________________________________________________________
const StvHit& StvHit::operator=(const StvHit & h)
{
  memcpy(mBeg,h.mBeg,mEnd-mBeg+1);
  return *this;
}

//_____________________________________________________________________________
StvHit::~StvHit()
{}

//_____________________________________________________________________________
/// Convenience method to perform a rotation
/// around the z axis
void StvHit::rotate(double alpha)
{
  assert(!mDetector);
static float rotMx[3][3]={{1,0,0},{0,1,0},{0,0,1}}, s[6];

  double ca = cos(alpha);
  double sa = sin(alpha);
  rotMx[0][0] = ca; rotMx[0][1] = sa;
  rotMx[1][0] =-sa; rotMx[1][1] = ca;

  double x = rotMx[0][0]*mLoc[0] + rotMx[0][1]*mLoc[1];
  double y = rotMx[1][0]*mLoc[0] + rotMx[1][1]*mLoc[1];
  mLoc[0] = x; mLoc[1] = y;

// 		S=R*S*Rt
  memcpy(s,mErr,sizeof(s));
  TCL::trasat(rotMx[0],s,mErr,3,3);
}

//_____________________________________________________________________________
void StvHit::setError(const float matrix[6])
{
  memcpy(mErr,matrix,sizeof(mErr));
}  

//_____________________________________________________________________________
/*! Streamer for StvHit objects. */
ostream& operator<<(ostream& os, const StvHit& hit)
{
  return os <<"L:"<<hit.x()  <<" "<<hit.y()  <<" "<<hit.z()
	    <<"G:"<<hit.x_g()[0]<<" "<<hit.x_g()[1]<<" "<<hit.x_g()[2];
}
//_____________________________________________________________________________
void StvHit::reset()
{
  memset(mBeg,0,mEnd-mBeg+1);
static unsigned int myCount=0;  
  mCount = ++myCount;
}


//_____________________________________________________________________________
void StvHit::setGlobal(const StHitPlane *detector,
		       const void *stHit,
		       const float *gx,
		       const float *gErr)
{
  mDetector = detector;
  memcpy(mGlo,gx,sizeof(mGlo));
  if (gErr) { memcpy(mErr,gErr,sizeof(mErr));}
  msthit = stHit;
  if (!mDetector) return;
  const Mtx33F_t &dir = mDetector->GetDir(mGlo);
  const    float *org = mDetector->GetOrg(mGlo); 
  float dif[3]={ mGlo[0]-org[0],mGlo[1]-org[1],mGlo[2]-org[2]};
  for (int i=0;i<3;i++) {
    mLoc[i]=dir[i][0]*dif[0]+dir[i][1]*dif[1]+dir[i][2]*dif[2];}
  return;
}









