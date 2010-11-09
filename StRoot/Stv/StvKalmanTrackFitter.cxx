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

//===================================================
{
printf("STANDARD:\n");
        node->mPP[dir].print();
        THelixTrack hlx,dlx;
        double F[5][5],dL;
        preNode->mFP[1].get(&dlx);
        dL = dlx.Path(node->mFP[0].P);
	dlx.Move(dL,F);
        StvNodePars mypars;
        mypars.set(&dlx,node->mFP[0]._hz);

printf("ByHELIX:\n");
        mypars.print();

        double myF[5][5];
        mypars.convert(myF,F);


  enum {kH=0,kA,kC,kZ,kL};
        double dA[5],dB[5],dX[2];
        dX[0] = preNode->mFP[1]._x-preNode->mPP[0]._x;
        dX[1] = preNode->mFP[1]._y-preNode->mPP[0]._y;
        dA[kH] = -dX[0]*preNode->mPP[0]._sinCA+dX[1]*preNode->mPP[0]._cosCA;
        dA[kA] = preNode->mFP[1]._psi-preNode->mPP[0]._psi;
        dA[kC] = preNode->mFP[1]._curv-preNode->mPP[0]._curv;
        dA[kZ] = preNode->mFP[1]._z-preNode->mPP[0]._z;
        dA[kL] = atan(preNode->mFP[1]._tanl)-atan(preNode->mPP[0]._tanl);
        TCL::vmatl(F[0],dA,dB,5,5);
        mypars = node->mFP[0];
        mypars._x += -mypars._sinCA*dB[kH];
        mypars._y +=  mypars._cosCA*dB[kH];
        mypars._z += dB[kZ];
        mypars._psi += dB[kA];
        mypars._curv += dB[kC];
        mypars._ptin  = mypars._curv/mypars._hz;
        mypars._tanl  = tan(atan(mypars._tanl)+dB[kL]);
        mypars.ready();
printf("DerHELIX:\n");
        mypars.print();



}
//===================================================
        
//??        node->mPP[dir]+= ((preNode->mFP[dir]-preNode->mPP[1-dir])*node->mDer[dir]);
        node->mPE[dir] = preNode->mFE[dir]*node->mDer[dir];
        node->mFE[dir] = node->mPE[dir];
      }
//		Standard Kalman fit step.
      node->mFP[dir] = node->mPP[dir];
      node->mFE[dir] = node->mPE[dir];
      Xi2[0] = 3e33;
      if (!node->GetHit()) 		break;

      fitt->Set(node->mPP+dir,node->mPE+dir,node->mFP+dir,node->mFE+dir);
      fitt->Prep();
      Xi2[2] = fitt->Xi2(node->GetHit());
      if (Xi2[2] > kons->mXi2Hit) 	break;
      Xi2[0] = Xi2[2];

      fitt->Update();

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
      node->SetFit(node->mFP[dir],node->mFE[dir],2);
      node->SetXi2(Xi2[0]);
    }
  }
  return 0;
}
