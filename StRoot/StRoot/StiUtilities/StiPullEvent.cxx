#include <assert.h>
#include "StiPullEvent.h"
#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "StEvent/StEnumerations.h"
#include <assert.h>


//_____________________________________________________________________________
 StiPullEvent::StiPullEvent()
 :mTrksG("StiPullTrk",100)
 ,mTrksP("StiPullTrk",100)
 ,mHitsG("StiPullHit",100)
 ,mHitsP("StiPullHit",100)
 ,mHitsR("StiPullHit",100)
 {
 mHitsG.SetOwner(0); mHitsP.SetOwner(0); mHitsR.SetOwner(0);
 Clear();
 };
//_____________________________________________________________________________
void StiPullTrk::Clear(const char*)
{
   memset(mBeg,0,mEnd-mBeg+1);
}
//_____________________________________________________________________________
StiPullTrk::StiPullTrk()
{
   Clear();
}
//_____________________________________________________________________________
void StiPullTrk::Print(const char* option) const
{
  if (!option) option="";
  printf("StiPullTrk::Print(%s)\n",option);
}
 
 
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
  TClonesArray *hits = &mHitsG+gloPrim;
  int iHit = hits->GetLast()+1;
  StiPullHit *kHit = (StiPullHit*)hits->New(iHit);
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
void StiPullEvent::Add(StiPullTrk &trk,int gloPrim)
{
  TClonesArray *trks = &mTrksG+gloPrim;
  int iTrk = trks->GetLast()+1;
  StiPullTrk *kTrk = (StiPullTrk*)trks->New(iTrk);
  *kTrk = trk;
  assert(trk.mTrackNumber);
  mNTrks[gloPrim]++;
}
//_____________________________________________________________________________

const int *StiPullEvent::GetNHits() const
{
  return mNHits;
}
//_____________________________________________________________________________
void StiPullEvent::Clear(const char*)
{
  mHitsG.Clear();mHitsP.Clear();mHitsR.Clear();
  mTrksG.Clear();mTrksP.Clear();
  memset(mVtx,0,sizeof(mVtx));
  memset(mEtx,0,sizeof(mEtx));
  mRun=0; mEvt=0;
  memset(mNTrks,0,sizeof(mNTrks));
  memset(mNHits,0,sizeof(mNHits));
  
}
//_____________________________________________________________________________
void StiPullEvent::Finish()
{
static int nCall=0; nCall++;
  int iTkG=0,iTkP=0,iHiG=0,iHiP=0,idP=0,idG=0;
  StiPullTrk* trkG=0,*trkP =0;
  StiPullHit* hiG=0,*hiP =0;
  int nHiG = mHitsG.GetLast()+1;
  int nHiP = mHitsP.GetLast()+1;
  for (iTkG=0;iTkG<mNTrks[0];iTkG++) {// Loop over globs
    trkG=(StiPullTrk*)mTrksG[iTkG]; idG = trkG->mTrackNumber;
    idP=-1; trkP=0;
    if (iTkP<mNTrks[1]) {
      trkP=(StiPullTrk*)mTrksP[iTkP]; idP = trkP->mTrackNumber;
    }
    int vertex = 0;
    if (idG==idP) { //It is a primary
      vertex = trkP->mVertex;
      int nHits=0;
      for(;iHiP<nHiP; iHiP ++) { // Loop over prim hits
        hiP = (StiPullHit*)mHitsP[iHiP]; if (idP!=hiP->mTrackNumber) break;
        nHits++;
        hiP->mTrackNumber=iTkG+1; hiP->mVertex=vertex;
      }
      assert(nHits==trkP->nAllHits || nHits-1==trkP->nAllHits);
      trkP->mTrackNumber= iTkG+1; iTkP++;
    }//end It is a primary

    int nHits=0;
    for(;iHiG<nHiG; iHiG ++) { // Loop over glob hits
      hiG = (StiPullHit*)mHitsG[iHiG]; if (idG!=hiG->mTrackNumber) break;
      nHits++;
      hiG->mTrackNumber=iTkG+1; hiG->mVertex=vertex;
    }
    assert(nHits==trkG->nAllHits);
    trkG->mVertex = vertex; 
    trkG->mTrackNumber= iTkG+1; 
  }
}  
  
  
  
