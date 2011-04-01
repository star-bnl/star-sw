#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"

#include "TCernLib.h"
#include "StvKalmanTrackFitter.h"
#include "Stv/StvToolkit.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "Stv/StvFitter.h"
#include "Stv/StvConst.h"
#include "Stv/StvStl.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
ClassImp(StvKalmanTrackFitter)
#define DIST2(a,b) ((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])+(a[2]-b[2])*(a[2]-b[2]))



//_____________________________________________________________________________
StvKalmanTrackFitter::StvKalmanTrackFitter():StvTrackFitter("StvKalmanTrackFitter")
{
  memset(mBeg,0,mEnd-mBeg+1);
}  
//_____________________________________________________________________________
void StvKalmanTrackFitter::Clear(const char*)
{
 StvTrackFitter::Clear("");
}

//_____________________________________________________________________________
int  StvKalmanTrackFitter::Refit(StvTrack *trak,int dir)
{
///	refit or smouthe track, using the previous Kalman.
///     idir=0 moving from out to in
///     idir=1 moving from in to out 
static int nCall=0; nCall++;
static const double kBigErrFact = 10;

  StvFitter *fitt = StvFitter::Inst();
  StvConst  *kons = StvConst::Inst();

  StvNodeIter it,itBeg,itEnd;
  if (dir) { //fit in ==> out
    itBeg = trak->begin();        itEnd = trak->end();
  } else   {//fit out ==> in
    itBeg = trak->end(); --itBeg; itEnd = trak->begin();--itEnd;
  }

  double Xi2[3];
  StvNode *node,*preNode=0,*begNode=0;
  int iNode=0,skip=0;
  for (it=itBeg; it!=itEnd; (dir)? ++it:--it) {
    node = *it; iNode++; skip=1;
    do { //pseudo loop
      if (!begNode && node->GetXi2()<1e5) begNode = node; 
      if (!begNode) break;
      if (begNode==node) 	{// 1st node
        node->mPP[dir] = node->mFP[1-dir];
        node->mPE[dir].Set(node->mFE[1-dir],kBigErrFact);
        node->mFP[2] = node->mFP[1-dir];
        node->mFE[2] = node->mFE[1-dir];
        
      } else 		{ // normal node
        skip = 0;
        node->mPP[dir] = node->mFP[1-dir];
//????		ONLY FOR dir==1
        StvFitPars delPre = preNode->mFP[dir]-preNode->mPP[1-dir];
        if (delPre.Check()) 	return 1; 
        
        StvFitPars del    = delPre*preNode->mDer[dir];
        if (del.Check()) 	return 2; 
        node->mPP[dir]+= del;node->mFP[dir]=node->mPP[dir];

        
//??        node->mPP[dir]+= ((preNode->mFP[dir]-preNode->mPP[1-dir])*node->mDer[dir]);
        node->mPE[dir] = preNode->mFE[dir]*node->mDer[dir];
        node->mPE[dir].Add(preNode->mELossData,node->mPP[dir]);
        node->mFE[dir] = node->mPE[dir];
        if(node->mFE[dir].Check()) 	return 3; 
      }
//		Standard Kalman fit step.
      node->mFP[dir] = node->mPP[dir];
      node->mFE[dir] = node->mPE[dir];
      Xi2[0] = 0;
      StvHit *hit = node->GetHit();
      if (!hit) 		break;

      fitt->Set(node->mPP+dir,node->mPE+dir,node->mFP+dir,node->mFE+dir);
      fitt->Prep();
      Xi2[2] = fitt->Xi2(hit);
      if (Xi2[2] > kons->mXi2Hit) { node->SetHit(0); break;}
      Xi2[0] = Xi2[2];

      fitt->Update();
      node->mFP[2] = node->mFP[dir];
    } while(0);

    preNode = node; if (skip) continue;

    fitt->Set(node->mFP+0 ,node->mFE+0
             ,node->mFP+1 ,node->mFE+1
             ,node->mFP+2 ,node->mFE+2);
    Xi2[1] = fitt->Xi2(); 
    if (Xi2[1] < kons->mXi2Joi) {
      fitt->Update();
      node->SetXi2(Xi2[0]+Xi2[1]);}
    else {
      trak->CutTail(node);
      break;
//??      node->SetFit(node->mFP[dir],node->mFE[dir],2);
//??      node->SetXi2(Xi2[0]);
    }
  }
  return 0;
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Fit(const StvTrack *trak,const StvHit *vtx,StvNode *node)
{
static       StvToolkit *kit     = StvToolkit::Inst();
static const StvConst   *myConst =   StvConst::Inst();
static const double dca3dVertex = myConst->mDca3dVertex;
static StvFitter *fitt = StvFitter::Inst();

  const StvNode *lastNode = trak->GetNode(StvTrack::kDcaPoint);
  if (!lastNode) return 1;
  THelixTrack th;
  lastNode->GetFP().get(&th);
  lastNode->GetFE().Get(&th);
  const float *h = vtx->x();
  double d[3]={h[0],h[1],h[2]};
  double len = th.Path(d);
  double x[3];
  th.Eval(len,x);
  
  if (DIST2(d,x) > dca3dVertex*dca3dVertex) return 2;
  Mtx55D_t derivFit,derivHlx;
  if (node) {th.Move(len,derivHlx);} else {th.Move(len);}
  double Hz = kit->GetHz(th.Pos());
  StvNodePars par[2]; par[0].set(&th,Hz);
  StvFitErrs  err[2]; err[0].Set(&th,Hz);
  fitt->Set(par+0,err+0,par+1,err+1);
  fitt->Prep();
  mXi2 = fitt->Xi2(vtx);
  if (mXi2>myConst->mXi2Vtx) return 3;
  if (!node) return 0;
  
  fitt->Update();
  node->SetPre(par[0],err[0],0);
  node->SetFit(par[1],err[1],0);
  par[1].convert(derivFit,derivHlx);
  node->SetDer(derivHlx,0);
  return 0;
}   
  
  
