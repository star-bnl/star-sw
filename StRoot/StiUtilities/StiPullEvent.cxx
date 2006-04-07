#include "StiPullEvent.h"
#include "TCL.h"

ClassImp(StiPullHit)
ClassImp(StiPullEvent)

//_____________________________________________________________________________
 StiPullEvent::StiPullEvent()
 :mHitsG("StiPullHit",100),mHitsP("StiPullHit",100)
 {
 mHitsG.SetOwner(0);mHitsP.SetOwner(0);
 };
 
 
//_____________________________________________________________________________
void StiPullHit::Clear(const char*)
{
   memset(mBeg,0,mEnd-mBeg+1);
}
//_____________________________________________________________________________
StiPullHit::StiPullHit()
{
   Clear();
}
//_____________________________________________________________________________
void StiPullHit::Print(const char* option) const
{
  if (!option) option="";
  printf("StiPullHit::Print(%s)\n",option);
  printf("mTrackNumber=%d mHardwarePosition=%u \n"
        ,mTrackNumber,mHardwarePosition);
  printf("mNormalRefAngle=%g mNormaYOffset=%g mZCenter=%g\n"
        ,mNormalRefAngle,mNormalYOffset,mZCenter);

  printf("lXHit=%g \tlYHit %g(%g)\tlZHit %g(%g)\n"
        ,lXHit
	,lYHit,lYHitErr
	,lZHit,lZHitErr);

  printf("lXFit=%g \tlYFit %g(%g)\tlZFit %g(%g)\n"
        ,lXFit
	,lYFit,lYFitErr
	,lZFit,lZFitErr);

  printf("lYPul %g(%g)\tlZPul %g(%g)\n"
	,lYPul,lYPulErr
	,lZPul,lZPulErr);


  printf("gRHit=%g \tgPHit %g(%g)\tgZHit %g(%g)\n"
        ,gRHit
	,gPHit,gPHitErr
	,gZHit,gZHitErr);

  printf("gRFit=%g \tgPFit %g(%g)\tgZFit %g(%g)\n"
        ,gRFit
	,gPFit,gPFitErr
	,gZFit,gZFitErr);

  printf("gPPul %g(%g)\tgZPul %g(%g)\n"
	,gPPul,gPPulErr
	,gZPul,gZPulErr);


}
//_____________________________________________________________________________
int StiPullHit::TestIt()
{
  return 0;
}   
//_____________________________________________________________________________
void StiPullEvent::Add(StiPullHit &hit,int gloPrim)
{
  TClonesArray *hits = (!gloPrim)? &mHitsG:&mHitsP;
  int iHit = hits->GetLast()+1;
  StiPullHit *kHit = (StiPullHit*)hits->New(iHit);
  *kHit = hit;
}
//_____________________________________________________________________________
void StiPullEvent::Clear(const char*)
{
  mHitsG.Clear();mHitsP.Clear();
  memset(mVtx,0,sizeof(mVtx));
  memset(mEtx,0,sizeof(mEtx));
  mRun=0; mEvt=0;
}
  
  
  
  
  
