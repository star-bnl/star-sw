#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"

#include "TCernLib.h"
#include "StvKalmanTrackFitter.h"
#include "Stv/StvToolkit.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "StvUtil/StvDebug.h"
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
///     dir=0 moving from out to in
///     dir=1 moving from in to out 
static int nCall=0; nCall++;
static const double kBigErrFact = 10;

static StvFitter *fitt = StvFitter::Inst();
static StvConst  *kons = StvConst::Inst();

  StvNode *breakNode = 0;
  trak->MakeFitTally();
  StvNodeIter it,itBeg,itEnd;
  if (dir) { //fit in ==> out
    itBeg = trak->begin();        itEnd = trak->end();
  } else   {//fit out ==> in
    itBeg = trak->end(); --itBeg; itEnd = trak->begin();--itEnd;
  }


  double myXi2;
  StvNode *node=0,*preNode=0;
  int iNode=0,nFitLeft=0;

  for (it=itBeg; it!=itEnd; (dir)? ++it:--it) {//Main loop
    preNode=node;
    assert(!preNode || !preNode->Check());
    node = *it; iNode++;

enum myCase {kLeft=1,kRite=2,kHit=4};
    int kase = 0;
    if (nFitLeft) 						kase|=kLeft;
    if (node->FitTally()[1-dir]  ) 				kase|=kRite;
    if (node->GetHit() && (node->GetXi2(1-dir)<1000 || nFitLeft>2))kase|=kHit;
// kLeft 		= left fits only, no hit
// kLeft+kHit 		= Left fits only, now hit, 
// kLeft+kGood 		= Left fits only, now Good Hit, 
// kRite 		= No left, no hit, rite fits only
// kLeft+kRite 		= left fits, no hit, rite fits
// kHit+kRite 		= No left fits, now hit, rite fits
// kLeft+kRite+kHit 	= Left fits,now hit, rite fits
// kLeft+kRite+kGood 	= Left fits,now good Hit, Rite fits
    node->SetXi2(3e33,dir);
    switch (kase) {// 1st switch, fill prediction

      default: assert(0 && "Wrong Case1");

      case kLeft: 		// Left fits only, no  Hit
      case kLeft+      kHit: 	// Left fits only, now Hit
      case kLeft+kRite     : 	// Left fits, no Hit, Rite fits
      case kLeft+kRite+kHit: 	// Left fits,now Hit, Rite fits
      {
        node->mPP[dir] = node->mFP[1-dir];
        StvFitPars delPre = preNode->mFP[dir]-preNode->mPP[1-dir];
        breakNode = (dir)? node:preNode;
        if (delPre.Check()) { StvDebug::Count("delPre.Check");
	                      /*trak->CutTail(breakNode);*/ return 1;}
        StvFitPars del    = delPre*preNode->mDer[dir];
        if (del.Check())    { StvDebug::Count("del.Check");
	                      /*trak->CutTail(breakNode);*/ return 2;}; 
        node->mPP[dir]+= del;node->mFP[dir]=node->mPP[dir];
//        node->mPP[dir]+= ((preNode->mFP[dir]-preNode->mPP[1-dir])*node->mDer[dir]);
        node->mPE[dir] = preNode->mFE[dir]*node->mDer[dir];
        node->mPE[dir].SetHz(node->mPP[dir]._hz);
        node->mPE[dir].Add(preNode->mELossData,node->mPP[dir]);
        if(node->mPE[dir].Check()) { StvDebug::Count("mFE[dir].Check");
	                             /*trak->CutTail(breakNode);*/ return 3;}; 
        break;
      }  

      case kRite: 	// No left, no  Hit, rite fits 
      {
        node->SetPre(node->mFP[2],node->mFE[2],dir); 	
        break;
      }
      case kRite|kHit: // No left, now Hit, rite fits 
      {
        node->mPP[dir] = node->mFP[2]; 	
        node->mPE[dir].Set(node->mFE[2],kBigErrFact);
        break;
      }
    }//End 1st switch


    switch (kase) {// 2nd switch, fill fit

      case kLeft: 	// Left fits only, no  Hit
      case kRite: 	// Rite fits only, no  Hit
      case kLeft+kRite: // Left fits, no Hit, Rite fits
      {
        node->SetFit(node->mPP[dir],node->mPE[dir],dir); 
        break;
      }

      case kLeft+      kHit: // Left fits only, now Hit
      case kRite+      kHit: // No left, now Hit, rite fits 
      case kLeft+kRite+kHit: // Left fits,now Hit, Rite fits
      {
	StvHit *hit = node->GetHit();
	fitt->Set(node->mPP+dir,node->mPE+dir,node->mFP+dir,node->mFE+dir);
	fitt->Prep();

	myXi2 = fitt->Xi2(hit);
	if (myXi2> kons->mXi2Hit)  	{
	  node->SetFit(node->mPP[dir],node->mPE[dir],dir); 	
          kase -= kHit;
	} else {
          nFitLeft++; kase|=kLeft;
	  node->SetXi2(myXi2,dir);
	  fitt->Update();
        }
        break;
      }
      default: assert(0 && "Wrong Case2");

    }//end 2nd switch

    switch (kase) {// 3rd switch,Join

      case kLeft+kRite     : 	// Left fits, no Hit, Rite fits
      case kLeft+kRite+kHit: 	// Left fits,now Hit, Rite fits
      {
	breakNode = (dir)? node:preNode;
	fitt->Set(node->mFP+0 ,node->mFE+0
        	 ,node->mFP+1 ,node->mFE+1
        	 ,node->mFP+2 ,node->mFE+2);
	node->SetXi2(3e33,2);
	myXi2 = fitt->Xi2(); 
	if (myXi2 < kons->mXi2Joi) {
	  fitt->Update();
	  node->SetXi2(myXi2,2);}
	else {
	  const char *key = (dir)? "Join111.Fail":"Join000.Fail";
	  StvDebug::Count(key,myXi2); 
	  /*trak->CutTail(breakNode);*/ return 4;
	}
      }

    }//End 3rd case

  }//endMainLoop
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
  
  
