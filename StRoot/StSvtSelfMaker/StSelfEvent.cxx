#include "StSelfEvent.h"
#include "TCL.h"

ClassImp(StSelfHit)
ClassImp(StSelfEvent)

//_____________________________________________________________________________
 StSelfEvent::StSelfEvent():mHits("StSelfHit",100)
 {mHits.SetOwner(0);};
 
 
//_____________________________________________________________________________
StSelfHit::StSelfHit()
{
   int n = ((char*)&mId - (char*)&mTrackNumber)+sizeof(mId);
   memset(&mTrackNumber,0,n);
}
//_____________________________________________________________________________
void StSelfHit::Print(const char* option) const
{
  if (!option) option="";
  printf("StSelfHit::Print(%s)\n",option);
  printf("mTrackNumber=%d mHardwarePosition=%u mId=%d\n"
        ,mTrackNumber,mHardwarePosition,mId);
  printf("mNormalRefAngle=%g mNormaYOffset=%g mZCenter=%g\n"
        ,mNormalRefAngle,mNormalYOffset,mZCenter);

  printf("mXg=%g %g %g \tmXl=%g %g %g\n"
        ,mXg[0],mXg[1],mXg[2]
        ,mXl[0],mXl[1],mXl[2]);

  printf("mFg=%g %g %g \tmFl=%g %g %g\n"
        ,mFg[0],mFg[1],mFg[2]
        ,mFl[0],mFl[1],mFl[2]);

  printf("mDg=%g %g %g \tmDl=%g %g %g\n"
        ,mDg[0],mDg[1],mDg[2]
        ,mDl[0],mDl[1],mDl[2]);

}
//_____________________________________________________________________________
int StSelfHit::TestIt()
{
  if (mHardwarePosition) {
    assert(mXl[0]>=0);
    assert(mFl[0]>=0);
  } else {
    assert(fabs(mXl[0])<1);
    assert(fabs(mXl[1])<1);
    assert(fabs(mXl[2])<100);
    assert(fabs(mXg[0])<1);
    assert(fabs(mXg[1])<1);
    assert(fabs(mXg[2])<100);
  }
  return 0;
}   
//_____________________________________________________________________________
int StSelfHit::Prepare()
{
//  Some changes for ttree
  TCL::vsub(mFg,mXg,mFg,3);
  TCL::vsub(mFl,mXl,mFl,3);
  mXl[1] -=mNormalYOffset;
  mXl[2] -=mZCenter;
  return 0;
}
