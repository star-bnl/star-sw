#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"

#include "StvSeedFinder.h"
#include "StvKalmanTrackFinder.h"

#include "StvToolkit.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "Stv/StvConst.h"
#include "Stv/StvDiver.h"
#include "Stv/StvHitter.h"
#include "Stv/StvFitter.h"
#include "Stv/StvTrackFitter.h"
#include "Stv/StvConst.h"
#include "THelixTrack.h"
#include "Stv/StvDraw.h"
#include "Stv/StvStl.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
ClassImp(StvKalmanTrackFinder)
//_____________________________________________________________________________
StvKalmanTrackFinder::StvKalmanTrackFinder(const char *name):StvTrackFinder(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
  mDive = new StvDiver("KalmanTrackFinder");
  mDive->Init();
  mHitter = new StvHitter();
}  
//_____________________________________________________________________________
void StvKalmanTrackFinder::Clear(const char*)
{
 StvTrackFinder::Clear("");
}
//_____________________________________________________________________________
void StvKalmanTrackFinder::Reset()
{
}    

//_____________________________________________________________________________
int StvKalmanTrackFinder::FindTracks()
{
static int trkShow=0;
static int kitShow=0;
//  DoShow(2);
  int nTrk = 0;
  StvToolkit *kit = StvToolkit::Inst();
  StvSeedFinder *sf = kit->SeedFinder();
  while ((mSeedHelx = sf->NextSeed())) 
  {
if (sf->DoShow())  sf->Show();

    StvTrack *stk = FindTrack(0);
    if (!stk) continue;
    nTrk++;
if (trkShow)    stk->Show();
//		Refit track   
    int ans = Refit(stk,1);

    if (!ans) kit->GetTracks().push_back(stk);
  }
if (kitShow)  kit->Show();
if (sf->DoShow()>1)  sf->ShowRest();
if (StvDraw::Jnst()) StvDraw::Wait();


  return nTrk;
}
//_____________________________________________________________________________
StvTrack *StvKalmanTrackFinder::FindTrack(int idir)
{

static int nCall=0; nCall++;
static const double P2CUT = 0.003; 	// Geant3 cut for too small momentum
static const double CUCUT = 0.1; 	// Cut for too big curvature
static const StvConst *myConst = StvConst::Inst();
StvToolkit *kit = StvToolkit::Inst();

StvTrack *myTrak = kit->GetTrack();
StvNodePars par[2];
StvFitErrs  err[2];
int mySkip=0;
StvHitCount hitCount;
   

  double Hz = kit->GetHz(mSeedHelx->Pos());
  par[1].set(mSeedHelx,Hz);
  par[1].reverse();
  StvFitter *fitt = StvFitter::Inst();
  StvConst  *kons = StvConst::Inst();
  fitt->Set(par, err, par+1,err+1);
//  	check too big curvature
  if (fabs(par[1]._curv)>CUCUT) return 0;

//  	check P too small
  { double t = par[1]._tanl, pti = par[1]._ptin;
    if ((t*t+1.)< P2CUT*pti*pti) return 0;
  }
  err[1].Reset();err[1].SetHz(par[1]._hz);
  err[0]=err[1];
  fShowTrak.clear();
  mHitter->Reset();
Mtx55D_t derivFit;
  int idive = 0,nNode=0;
  double totLen=0;
  mDive->Reset();
  par[0]=par[1]; err[0]=err[1];
  mDive->Set(par+0,err+0,idir);
  mDive->Set(par+1,err+1,&derivFit);
  while(idive==0) {

    if (!mySkip) { 
      mySkip = hitCount.Skip();
      if (mySkip) mDive->SetSkip();
    }
    par[0]=par[1]; err[0]=err[1];
//============================
    idive = mDive->Dive();
//============================

    totLen+=mDive->GetLength();
    nNode++; 
    par[0]=par[1]; err[0]=err[1];
    if (fabs(par[0]._z)  > myConst->mZMax  ) break;
    if (par[0].getRxy()  > myConst->mRxyMax) break;


    assert(idive || !err[0].Check("AfterDive"));
//    assert(         !par[0].check("AfterDive"));
    float gate[2]={3,3};   
    const StvHits *localHits = mHitter->GetHits(par,gate); 

if (DoShow()) {
//  double r = par[0].getRxy();
    printf("%3d Len=%g XY=%g %g Path=%s\n",nNode,totLen,par[1]._x,par[1]._y
          ,StTGeoHelper::Inst()->GetPath());
    fShowTrak+=&par[0]._x;
}//EndDoShow
//	Create and add node
    StvNode *node = kit->GetNode();      
    myTrak->push_front(node);

//Testik(deriv);

//    assert(!idive || !par[1].check("FindTrack.1"));
    node->SetPre(par[1],err[1],idir);
    node->SetDer(derivFit,idir);


    if (idive) node->SetType(StvNode::kDcaNode);

    if (!localHits)	 continue;//Never hits in node 
    node->SetHitPlane(mHitter->GetHitPlane());
    if (!localHits->size()) {//No hits in node
      hitCount.AddNit(); continue;
    } 
    fitt->Prep();
    double minXi2 = kons->mXi2Hit; StvHit *minHit=0; int minIdx = -1;
    for (int ihit=0;ihit<(int)localHits->size();ihit++) {
      StvHit *hit = (*localHits)[ihit];
      double myXi2 = fitt->Xi2(hit);
      if (myXi2 > minXi2) continue;
      minXi2 = myXi2; minHit = hit; minIdx = ihit;
    }

    if (! minHit) {
      hitCount.AddNit();
    } else {
      hitCount.AddHit();

      (*((StvHits*)localHits))[minIdx]=0;
      double myXi2 = fitt->Xi2(minHit);
      assert(fabs(minXi2-myXi2)<1e-5);
      int iuerr = fitt->Update();if (iuerr){}; 
      assert(err[0].mHH>err[1].mHH || err[0].mZZ>err[1].mZZ);
      assert(err[0].mHH*err[0].mZZ > err[1].mHH*err[1].mZZ);


      fShowTrak+=&par[1]._x;

      node->SetHit(minHit);
      node->SetXi2(myXi2);
      node->SetHE(fitt->GetHitErrs());
      assert(!par[1].check("AfterFitter"));
      assert(!err[1].Check("AfterFitter"));
      node->SetFit(par[1],err[1],idir);
    }

if (DoShow()) {
//??    fShowFreeHits+=*localHits;
    if (minHit)    fShowTrakHits.push_back(minHit);
    printf("minXi2 = %g\n",minXi2);
}


  } // End Dive&Fitter loop 


if (hitCount.Reject()) {
  myTrak->ReleaseHits();myTrak->unset();
  kit->FreeTrack(myTrak);myTrak=0; }
  
if (DoShow()) { Show();}



  return myTrak;

}
//_____________________________________________________________________________
//_____________________________________________________________________________
int StvKalmanTrackFinder::Refit(StvTrack *tk,int idir)
{
StvTrackFitter::Inst()->Refit(tk,idir);
return 0;
}

