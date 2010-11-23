#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"

#include "TCernLib.h"
#include "StvKalmanTrackFitter.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "Stv/StvFitter.h"
#include "Stv/StvConst.h"
#include "Stv/StvStl.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
ClassImp(StvKalmanTrackFitter)
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
        StvFitPars del    = delPre*preNode->mDer[dir];
        node->mPP[dir]+= del;node->mFP[dir]=node->mPP[dir];

        
//??        node->mPP[dir]+= ((preNode->mFP[dir]-preNode->mPP[1-dir])*node->mDer[dir]);
        node->mPE[dir] = preNode->mFE[dir]*node->mDer[dir];
        node->mPE[dir].Add(preNode->mELossData,node->mPP[dir]);
        node->mFE[dir] = node->mPE[dir];
        
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
      if (Xi2[2] > kons->mXi2Hit) { hit->release(); node->SetHit(0); break;}
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
