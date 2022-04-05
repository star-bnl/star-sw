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
#include "StvUtil/StvHitErrCalculator.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"


//_____________________________________________________________________________
StvHit::StvHit()
{
   reset();
}

// //_____________________________________________________________________________
// StvHit::StvHit(const StvHit &h) 
// {
//   memcpy(mBeg,h.mBeg,mEnd-mBeg+1);
// }
// 
// //_____________________________________________________________________________
// const StvHit& StvHit::operator=(const StvHit & h)
// {
//   memcpy(mBeg,h.mBeg,mEnd-mBeg+1);
//   return *this;
// }
// 
//_____________________________________________________________________________
StvHit::~StvHit()
{}
//_____________________________________________________________________________
void StvHit::reset()
{
  memset(mBeg,0,mEnd-mBeg+1);
  mMaxTimes = 1;
static unsigned int myCount=0;  
  mCount = ++myCount;
}
//_____________________________________________________________________________
void StvHit::set(const void *stHit,const float *gx)
{
  memcpy(mGlo,gx,sizeof(mGlo));
  msthit = stHit;
  return;
}
//_____________________________________________________________________________
double StvHit::err2() const
{
  return ((StvHitErrCalculator*)(mDetector->GetHitErrCalc()))->Trace(mGlo);
}
//_____________________________________________________________________________
int StvHit::detectorId() const 
{ return (int)detector()->GetDetId(); }
//_____________________________________________________________________________
void StvVertex::set(const float *x,const float matrix[6])
{
  memcpy(mGlo,x     ,sizeof(mGlo));
  memcpy(mErr,matrix,sizeof(mErr));
}  
//_____________________________________________________________________________
void StvVertex::reset()
{
  memset(mErr,0,sizeof(mErr)); mKount=0;
  StvHit::reset();
}
//_____________________________________________________________________________
 void StvHit::setTimesUsed(int n)
{
    if (!mDetector) return;
    mTimesUsed=n;
}
//_____________________________________________________________________________
 void StvHit::addTimesUsed()
{
    if (!mDetector) return;
    mTimesUsed++; 
    assert(mTimesUsed<=mMaxTimes);
}
//_____________________________________________________________________________
 void StvHit::subTimesUsed()
{
    if (!mDetector) return;
    mTimesUsed--; 
    assert(mTimesUsed>=0);
}










