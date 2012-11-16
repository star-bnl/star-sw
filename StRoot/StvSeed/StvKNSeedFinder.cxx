#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "TCernLib.h"
#include "TVector3.h"
#include "StvKNSeedFinder.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"
#include "Stv/StvHit.h"
#include "THelixTrack.h"
//#define APPROX_DEBUG
#ifdef APPROX_DEBUG
#include "TCanvas.h"
#include "TH1F.h"
#include "TProfile.h"
#endif //APPROX_DEBUG
#include "StvUtil/StvDebug.h"
#include "Stv/StvDraw.h"
void myBreak(int);
enum {kMinHits=5,kMaxHits = 10,kFstAng=88,kErrFakt=5,kLenFakt=5,kStpFakt=3};
static const double kFstTan = tan(kFstAng*M_PI/180);
static const double kMinCos = 0.1;

ClassImp(StvKNSeedFinder)
static const float TpcOuterDeltaR = 15, kTpcHitErr = 0.2;

//_____________________________________________________________________________
StvKNSeedFinder::StvKNSeedFinder(const char *name):StvSeedFinder(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
  fMultiHits	= new StMultiKeyMap(3);
  fMultiIter	= new StMultiKeyMapIter(0);
  f1stHitMap 	= new Stv1stHitMap;
  f1stHitMapIter= new Stv1stHitMapIter;
}  
//_____________________________________________________________________________
void StvKNSeedFinder::Clear(const char*)
{
  memset(mBeg,0,mMed-mBeg+1);
  f1stHitMap->clear();
  fMultiHits->Clear();
  *f1stHitMapIter = f1stHitMap->end();
  StvSeedFinder::Clear();
}
//_____________________________________________________________________________
void StvKNSeedFinder::Reset()
{
  memset(mBeg,0,mMed-mBeg+1);
  const StVoidArr *hitArr =  StTGeoHelper::Inst()->GetSeedHits();
  int nHits =  hitArr->size();
  for (int iHit=0;iHit<nHits;iHit++) {
    StvHit *stiHit = (StvHit*)(*hitArr)[iHit];
    const float *x = stiHit->x();
//    float r2 = x[0]*x[0] + x[1]*x[1]+ x[2]*x[2];
    float r2 = x[0]*x[0] + x[1]*x[1] + x[2]*x[2];
    f1stHitMap->insert(std::pair<float,StvHit*>(-r2, stiHit));
    fMultiHits->Add(stiHit,x);
  } 
  fMultiHits->MakeTree();
  *f1stHitMapIter = f1stHitMap->begin();
}    
//_____________________________________________________________________________
int StvKNSeedFinder::Again()
{
  *f1stHitMapIter = f1stHitMap->begin();
   return 1;
}
//_____________________________________________________________________________
const THelixTrack* StvKNSeedFinder::NextSeed()
{
static int nCall=0; nCall++;
StvDebug::Break(nCall);
  StvHit *fstHit; 

  for (;(*f1stHitMapIter)!=f1stHitMap->end();++(*f1stHitMapIter)) {//1st hit loop
    fstHit = (*(*f1stHitMapIter)).second;
    assert(fstHit);
    if (fstHit->timesUsed()) continue;

    fSeedHits.clear();
    mRej.Reset(fstHit->x());
    mRej.Prepare();
    fMultiIter->Set(fMultiHits->GetTop(),mRej.mLim[0],mRej.mLim[1]);
    mSel.Reset(fstHit->x(),fstHit);
    int nTotHits=0,nAccHits=0;

//		Add all near hits 
    for (StMultiKeyNode *node=0;(node = *(*fMultiIter)) ;++(*fMultiIter)) 
    { 
//		Search next hit 
      StvHit *nexHit = (StvHit*)node->GetObj();
      if (nexHit==fstHit)	continue;
      if (nexHit->timesUsed()) 	continue;
      nTotHits++;
      int ans = mRej.Reject(nexHit->x());
      if (ans) continue;
      nAccHits++;
      mSel.Add(nexHit->x(),nexHit);

    } //endMultiIter loop

    int nHits = mSel.Select();
    if (nHits < kMinHits) continue;
{
int myShow = StvDebug::Flag("StvKNShow");
if (myShow) mSel.Show();
}
    fSeedHits.clear();
    fSeedHits+=mSel.Get();
    const THelixTrack *hel =  Approx();
    if (hel) { fNSeeds[0]++; ++(*f1stHitMapIter); return hel;}	//Good boy
 //		Bad boy
    fNUsed[0] -= fSeedHits.size();


  }// end 1st hit loop
  fNSeeds[1]+=fNSeeds[0]; fNUsed[1]+= fNUsed[0];
  return 0;
}
//_____________________________________________________________________________
const StvHits *StvKNSeedFinder::GetHits() const 	
{
     return (const StvHits*)(void*)(&(mSel.Get()));
}     
//_____________________________________________________________________________
void StvKNSeedFinder::Show()  	
{
     mSel.Show();
}     
     
//_____________________________________________________________________________
void StvKNSeedFinder::FeedBack(int success)  	
{
  if (success) {
    StvDebug::Count("KNDis_MinEig_succ1",sqrt(mSel.mEigen[0])*57,mSel.mKNNDist*57);
  } else {
    StvDebug::Count("KNDis_MinEig_succ0",sqrt(mSel.mEigen[0])*57,mSel.mKNNDist*57);
  }
}     
  
