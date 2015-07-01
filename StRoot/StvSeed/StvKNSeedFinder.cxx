#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "TCernLib.h"
#include "TVector3.h"
#include "StMultiKeyMap.h"
#include "THelixTrack.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "StvSeed/StvSeedConst.h"

#ifndef __NOSTV__
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "StvKNSeedFinder.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvDebug.h"
#include "Stv/StvDraw.h"
#endif

#ifdef APPROX_DEBUG
#include "TCanvas.h"
#include "TH1F.h"
#include "TProfile.h"
#endif //APPROX_DEBUG
void myBreak(int);

#ifndef __NOSTV__
ClassImp(StvKNSeedFinder)
#endif

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
#ifndef __NOSTV__
  memset(mBeg,0,mMed-mBeg+1);
  std::map<double,StvHit*> myMap;
  const StVoidArr *hitArr =  StTGeoProxy::Inst()->GetSeedHits();
  int nHits =  hitArr->size();
  for (int iHit=0;iHit<nHits;iHit++) {
    StvHit *hit = (StvHit*)(*hitArr)[iHit];
    if (hit->timesUsed()) continue;
    const float *x = hit->x();
    double qwe = x[0]+300*(x[1]+300*x[2]);
    myMap[qwe]=hit;
  }  

  for (std::map<double,StvHit*>::const_iterator it=myMap.begin()
    ;it != myMap.end();++it) {
    StvHit *hit = (*it).second;
    const float *x = hit->x();
    float r2 = x[0]*x[0] + x[1]*x[1] + 1e-2*x[2]*x[2];
    f1stHitMap->insert(std::pair<float,StvHit*>(-r2, hit));
    fMultiHits->Add(hit,x);
  } 
  fMultiHits->MakeTree();
  *f1stHitMapIter = f1stHitMap->begin();
#endif
}    
//_____________________________________________________________________________
int StvKNSeedFinder::Again(int)
{
  *f1stHitMapIter = f1stHitMap->begin();
   return 1;
}
//_____________________________________________________________________________
const THelixTrack* StvKNSeedFinder::NextSeed()
{
static int nCall=0; nCall++;
  StvHit *fstHit; 

  for (;(*f1stHitMapIter)!=f1stHitMap->end();++(*f1stHitMapIter)) {//1st hit loop
    fstHit = (*(*f1stHitMapIter)).second;
    assert(fstHit);
    if (fstHit->timesUsed()) continue;
    fSeedHits.clear();
    const float *hPos = fstHit->x();
    float Rxy2 = hPos[0]*hPos[0]+hPos[1]*hPos[1];
    const StHitPlane *hp = fstHit->detector();
    float lay = hp->GetLayer();
    float hDir[3]; 
    TCL::ucopy(hp->GetDir(hPos)[0],hDir,3);
    if (TCL::vdot(hDir,hPos,3)>0) { TCL::vscale(hDir,-1.,hDir,3);}
//    mRej.Reset(hPos,hDir,lay*kMaxHits*3);
    mRej.Reset(hPos);
    mRej.Prepare();
    fMultiIter->Set(fMultiHits->GetTop(),mRej.mLim[0],mRej.mLim[1]);
    mSel.Reset(fstHit->x(),fstHit);
    int nTotHits=0,nAccHits=0;

//		Add all near hits 
    for (StMultiKeyNode *node=0;(node = *(*fMultiIter)) ;++(*fMultiIter)) 
    { 
//		Search next hit 
      StvHit *nexHit = (StvHit*)node->GetObj();
      if (nexHit==fstHit)		continue;
      if (nexHit->timesUsed()) 		continue;
      if (nexHit->detector()==hp)	continue;
      const float *f = nexHit->x();
      if (f[0]*f[0]+f[1]*f[1]>=Rxy2)  	continue;
      nTotHits++;
      
      int ans = mRej.Reject(nexHit->x());
      if (ans) continue;
      nAccHits++;

      mSel.Add(nexHit->x(),nexHit);

    } //endMultiIter loop

#if 0
    StMultiKeyMapIter myMultiIter(fMultiHits->GetTop(),0,0);
    int myTotHits = 0,myAccHits=0;
    for (StMultiKeyNode *node=0;(node = *(myMultiIter)) ;++(myMultiIter)){ 
      StvHit *nexHit = (StvHit*)node->GetObj();
      if (nexHit==fstHit)		continue;
      if (nexHit->timesUsed()) 		continue;
      if (nexHit->detector()==hp)	continue;;
      const float *x = nexHit->x();
      int jk=0;
      for (int j=0;j<3;j++) {
        jk = 1;
        if (x[j]<mRej.mLim[0][j]) break;
        if (x[j]>mRej.mLim[1][j]) break;
        jk = 0;
      }
      if (jk) continue;
      myTotHits++;
      int ans = mRej.Reject(nexHit->x());
      if (ans) continue;
      myAccHits++;

    } //endMultiIter loop

    assert (abs(nTotHits-myTotHits)<=1);
    assert (abs(nAccHits-myAccHits)<=1);
#endif //0

    int nHits = mSel.Select();
    if (nHits < kMinHits) continue;
{
int myShow = StvDebug::iFlag("StvKNShow");
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
  
