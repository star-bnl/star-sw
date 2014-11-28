#include "StiHitContino.h"
#include "StMessMgr.h"
//______________________________________________________________________________
void StiHitContino::Reset()
{
   memset(this,0,sizeof(StiHitContino));
   mChi2[0]=1e61;
}
//______________________________________________________________________________
void StiHitContino::Add(StiHit *hit,Double_t chi2,Double_t detr)
{
   Int_t i=0;
   for (;i<kMaxSize;i++) {
     if (!mHits[i]) 		break;
     if (chi2 > mChi2[i]) 	continue;
     for (Int_t jr = kMaxSize-2;jr>=i;jr--) 
       {mHits[jr+1]=mHits[jr]; mChi2[jr+1]=mChi2[jr];mDetr[jr+1]=mDetr[jr];}
     				break;
   }
   if (i>=kMaxSize) 		return;
   mHits[i]=hit; mChi2[i]=chi2;mDetr[i]=detr;
}
//______________________________________________________________________________
void StiHitContino::Print(const char* tit) const
{
  if (!tit || !*tit) tit ="print()";
  Int_t n=NHits();	
  LOG_DEBUG << Form(" ****	StiHitContino::%s nHits=%d",tit,n)<< endm;
  for (Int_t i=0;i<n;i++) {
    LOG_DEBUG << Form("%3d - hit=%p chi2 = %g",i,(void*)mHits[i],mChi2[i]);}
    LOG_DEBUG << endm;
}
//______________________________________________________________________________
Int_t StiHitContino::NHits() const { 
  Int_t n=0; 
  for(Int_t i=0;i<kMaxSize;i++) {if (mHits[i]) n++;}; 
  return n;
}	
//______________________________________________________________________________

