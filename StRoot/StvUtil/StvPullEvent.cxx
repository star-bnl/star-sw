#include <assert.h>
#include "StvPullEvent.h"
#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "StEvent/StEnumerations.h"
#include <assert.h>

ClassImp(StvPullTrk)
ClassImp(StvPullHit)
ClassImp(StvPullEvent)

//_____________________________________________________________________________
 StvPullEvent::StvPullEvent()
 :mTrksG("StvPullTrk",100)
 ,mTrksP("StvPullTrk",100)
 ,mHitsG("StvPullHit",100)
 ,mHitsP("StvPullHit",100)
 ,mHitsR("StvPullHit",100)
 {
 mHitsG.SetOwner(0); mHitsP.SetOwner(0); mHitsR.SetOwner(0);
 Clear();
 };
//_____________________________________________________________________________
void StvPullTrk::Clear(const char*)
{
   memset(mBeg,0,mEnd-mBeg+1);
}
//_____________________________________________________________________________
StvPullTrk::StvPullTrk()
{
   Clear();
}
//_____________________________________________________________________________
void StvPullTrk::Print(const char* option) const
{
  if (!option) option="";
  printf("StvPullTrk::Print(%s)\n",option);
}
 
 
//_____________________________________________________________________________
void StvPullHit::Clear(const char*)
{
   memset(mBeg,0,mEnd-mBeg+1);
}
//_____________________________________________________________________________
StvPullHit::StvPullHit()
{
   Clear();
}
//_____________________________________________________________________________
void StvPullHit::Print(const char* option) const
{
  if (!option) option="";
  printf("StvPullHit::Print(%s)\n",option);

  printf("lYHit %g(%g)\tlZHit %g(%g) lLen=%g\n"
	,lYHit,lYHitErr
	,lZHit,lZHitErr,lLen);


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
int StvPullHit::TestIt()
{
  return 0;
}   
//_____________________________________________________________________________
void StvPullEvent::Add(StvPullHit &hit,int gloPrim)
{
  TClonesArray *hits = &mHitsG+gloPrim;
  int iHit = hits->GetLast()+1;
  StvPullHit *kHit = (StvPullHit*)hits->New(iHit);
  *kHit = hit;
  if (gloPrim) 	return;
  if (!hit.mDetector)	return;
  int i = 4;
  if (hit.mDetector==kTpcId) i=1;
  if (hit.mDetector==kSvtId) i=2;
  if (hit.mDetector==kSsdId) i=3;
  if (hit.mDetector==kPxlId) i=4;
  if (hit.mDetector==kIstId) i=5;
  if (!i || i>3)return;
  ++mNHits[i-1];
}
//_____________________________________________________________________________
void StvPullEvent::Add(StvPullTrk &trk,int gloPrim)
{
  TClonesArray *trks = &mTrksG+gloPrim;
  int iTrk = trks->GetLast()+1;
  StvPullTrk *kTrk = (StvPullTrk*)trks->New(iTrk);
  *kTrk = trk;
  assert(trk.mTrackNumber);
  mNTrks[gloPrim]++;
}
//_____________________________________________________________________________

const int *StvPullEvent::GetNHits() const
{
  return mNHits;
}
//_____________________________________________________________________________
void StvPullEvent::Clear(const char*)
{
  mHitsG.Clear();mHitsP.Clear();mHitsR.Clear();
  mTrksG.Clear();mTrksP.Clear();
  memset(mVtx,0,sizeof(mVtx));
  memset(mEtx,0,sizeof(mEtx));
  mRun=0; mEvt=0;
  memset(mNTrks,0,sizeof(mNTrks)+sizeof(mNHits));
  
}
//_____________________________________________________________________________
void StvPullEvent::Finish()
{
static int nCall=0; nCall++;
  int iTkG=0,iTkP=-1,iHiG=0,iHiP=0,idP=0,idG=0;
  StvPullTrk* trkG=0,*trkP =0;
  StvPullHit* hiG=0,*hiP =0;
  int nHiG = mHitsG.GetLast()+1;
  int nHiP = mHitsP.GetLast()+1;

  do {
    iTkP++;
    if (iTkP<mNTrks[1]) { trkP=(StvPullTrk*)mTrksP[iTkP];idP=trkP->mTrackNumber;}
    else                { trkP=0; idP=0;}
    for(;iTkG<mNTrks[0];iTkG++) {
      int vertex = 0;
      trkG=(StvPullTrk*)mTrksG[iTkG]; idG = trkG->mTrackNumber;
      if (idP==idG) vertex = trkP->mVertex;
      trkG->mVertex = vertex;
      for(;iHiG<nHiG; iHiG ++) { 
        hiG = (StvPullHit*)mHitsG[iHiG]; if (idG!=hiG->mTrackNumber) break;
        hiG->mTrackNumber=iTkG; hiG->mVertex=vertex;
      }
      for(;iHiP<nHiP; iHiP ++) { 
        hiP = (StvPullHit*)mHitsP[iHiP]; if (idP!=hiG->mTrackNumber) break;
        hiP->mTrackNumber=iTkP; hiP->mVertex=vertex;
      }
    }
  } while(trkP);
}  
  
  
