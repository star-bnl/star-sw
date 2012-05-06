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
int  StvKalmanTrackFitter::Refit(StvTrack *trak,int dir,int mode)
{
///	refit or smouthe track, using the previous Kalman.
///     dir=0 moving from out to in
///     dir=1 moving from in to out 
///     mode=0 No join
///     mode=1 Join
///     fit direction is imagined from left to rite
static int nCall=0; nCall++;
//static const double kBigErrFact = 10;
static const double kBigErrFact = 16;

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
//VP    assert(!mode || !preNode || !preNode->Check());
    node = *it; iNode++;

enum myCase {kNull=0,kLeft=1,kRite=2,kHit=4};
    int kase = 0;
    if (nFitLeft) 				kase|=kLeft;
    if (mode && node->FitTally()[1-dir]) 	kase|=kRite;
    if (node->GetHit() && (!mode || node->GetXi2(1-dir)<1000 || nFitLeft>2))
    						kase|=kHit;
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

      case kNull: {	// Empty leading node
        node->SetPre(node->mFP[2],node->mFE[2],dir);
        assert(node->mPP[dir].isValid());
        break;
      }  
      case kLeft: 		// Left fits only, no  Hit
      case kLeft|      kHit: 	// Left fits only, now Hit
      case kLeft|kRite     : 	// Left fits, no Hit, Rite fits
      case kLeft|kRite|kHit: 	// Left fits,now Hit, Rite fits
      {

        int ret = Propagate(node,preNode,dir);
        if (ret) return ret;
        break;
      }  

      case kRite: 	// No left, no  Hit, rite fits 
      {
        node->SetPre(node->mFP[2],node->mFE[2],dir); 	
        break;
      }
      case kNull|kHit: // No left, now Hit, No rite  
      case kRite|kHit: // No left, now Hit, rite fits 
      {
        node->mPP[dir] = node->mFP[2]; 	
        node->mPE[dir].Set(node->mFE[2],kBigErrFact);
        break;
      }
    }//End 1st switch


    switch (kase) {// 2nd switch, fill fit

      case kNull: 	// No   fits  no  Hit
      case kLeft: 	// Left fits only, no  Hit
      case kRite: 	// Rite fits only, no  Hit
      case kLeft|kRite: // Left fits, no Hit, Rite fits
      {
        node->SetFit(node->mPP[dir],node->mPE[dir],dir); 
        break;
      }

      case kNull|      kHit: // Left fits only, now Hit
      case kLeft|      kHit: // Left fits only, now Hit
      case kRite|      kHit: // No left, now Hit, rite fits 
      case kLeft|kRite|kHit: // Left fits,now Hit, Rite fits
      {
	StvHit *hit = node->GetHit();
	fitt->Set(node->mPP+dir,node->mPE+dir,node->mFP+dir,node->mFE+dir);
	fitt->Prep();

	myXi2 = fitt->Xi2(hit);
	if (myXi2> kons->mXi2Hit)  	{
	  node->SetFit(node->mPP[dir],node->mPE[dir],dir); 	
	  node->SetXi2(1e11,dir);
          kase -= kHit;
	} else {
          nFitLeft++; kase|=kLeft;
	  node->SetXi2(myXi2,dir);
	  fitt->Update();
          node->mFP[2] = node->mFP[dir];
          node->mFE[2] = node->mFE[dir];
        }
        break;
      }
      default: assert(0 && "Wrong Case2");

    }//end 2nd switch
    if (!mode) continue;

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
	  node->SetXi2(myXi2,2);
          break;
	}
//		We got a problem, join is faled
//              if no guilty hit refit failed
	  const char *key = (dir)? "Join111.Fail":"Join000.Fail";
	  StvDebug::Count(key,myXi2); 
	  /*trak->CutTail(breakNode);*/ return 4;
	}

    }//End 3rd case

  }//endMainLoop
  return 0;
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Propagate(StvNode  *node,const StvNode *preNode,int dir)
{
  const StvNode *losNode = (dir)? preNode:node;
  do { //Attempt to propagate with linear approximation;
    node->mPP[dir] = node->mFP[1-dir];
    StvFitPars delPre = preNode->mFP[dir]-preNode->mPP[1-dir];
    if (delPre.TooBig(preNode->mFP[dir])) 	break;
    StvFitPars del    = delPre*preNode->mDer[dir];
    if (del.TooBig(node->mPP[dir]))    		break; 
    node->mPP[dir]+= del;node->mFP[dir]=node->mPP[dir];
//        node->mPP[dir]+= ((preNode->mFP[dir]-preNode->mPP[1-dir])*node->mDer[dir]);
    node->mPE[dir] = preNode->mFE[dir]*node->mDer[dir];
    node->mPE[dir].SetHz(node->mPP[dir]._hz);
    node->mPE[dir].Add(losNode->mELossData,node->mPP[dir]);
    if(node->mPE[dir].Check()) break; 
    return 0;
  } while(0);

//		Propagate with THelixTrack
  const double *P = node->mFP[1-dir].P;
  THelixTrack myHlx;
  preNode->mFP[dir].get(&myHlx);
  preNode->mFE[dir].Get(&myHlx);
  double dS = myHlx.Path(P);
  myHlx.Move(dS);
  double rho = myHlx.GetRho();
  rho += -rho*losNode->GetELoss().mdPPdL*dS;
  myHlx.Set(rho);
  node->mPP[dir].set(&myHlx,preNode->mFP[dir]._hz);
  node->mPE[dir].Set(&myHlx,preNode->mFP[dir]._hz);
  node->mPE[dir].Add(losNode->mELossData,node->mPP[dir]);
  if(node->mPE[dir].Check())  return 1; 
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
  mDca3 = DIST2(d,x);
  if (mDca3 > dca3dVertex*dca3dVertex) {
    StvDebug::Count("PrimDca3Rej",sqrt(mDca3));
    return 2;
  }
  Mtx55D_t derivFit,derivHlx;
  if (node) {th.Move(len,derivHlx);} else {th.Move(len);}
  double Hz = kit->GetHz(th.Pos());
  StvNodePars par[2]; par[0].set(&th,Hz);
  StvFitErrs  err[2]; err[0].Set(&th,Hz);
  fitt->Set(par+0,err+0,par+1,err+1);
  fitt->Prep();
  mXi2 = fitt->Xi2(vtx);
  if (mXi2>myConst->mXi2Vtx) {
    StvDebug::Count("PrimXi2Rej",mXi2);
    return 3;
  }
  if (!node) return 0;
  
  fitt->Update();
  node->SetPre(par[0],err[0],0);
  node->SetFit(par[1],err[1],0);
  par[1].convert(derivFit,derivHlx);
  node->SetDer(derivHlx,0);
  return 0;
}   
//_____________________________________________________________________________
THelixTrack* StvKalmanTrackFitter::GetHelix() const {return mHelx;}

//_____________________________________________________________________________
int StvKalmanTrackFitter::Helix(StvTrack *trak,int mode)
{
static int nCall=0;nCall++;
// mode.bit0 = use err
// mode.bit1 = update track
// mode.bit2 = print
// mode.bit3 = print+compare

  if (!mode  ) mode = 4;
  if ( mode&8) mode|= 4;
  mWorstXi2=0,mWorstNode = 0;mXi2 = 0;
  if (!mHelx) mHelx = new THelixFitter;
  mHelx->Clear();
  double Fhlx[5][5],Fstv[5][5];
  THelixFitter& hlx = *mHelx;
  StvNode *node=0,*preNode=0;
  for (StvNodeIter it=trak->begin();it!=trak->end(); ++it) {
    node = *it; 
    const StvHit *hit= node->GetHit();
    if (!hit) continue;
    hlx.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
    if(mode&1) {	//Account errors
      double cos2li = node->GetFP()._tanl; cos2li = (1+cos2li*cos2li);
      const double *rr = node->GetHE();    
      assert(rr[0]>0);
      assert(rr[2]>0);
      assert(rr[0]*rr[2]>rr[1]*rr[1]);
      hlx.AddErr( rr[0],rr[2]*cos2li);
    }
  }  
  mXi2 =hlx.Fit();
  if(mode&1) { hlx.MakeErrs(); assert(hlx.Emx()->Sign()>=0);}
  double dL = hlx.Path(trak->front()->GetFP().P);
  hlx.Move(dL);

  int nHits = 0;
  node=0;
  double dHit[3],tstXi2=0,myXi2=0;

//		Loop for Print,compare & update
  double totLen=0;
  THelixTrack myHlx(hlx);
  int iNode = -1;
  for (StvNodeIter it=trak->begin();it!=trak->end(); ++it) {
    iNode++;preNode=node; node = *it;
    StvNodePars sFP = node->GetFP(2);
    const StvHit *hit = node->GetHit();
    const double *X = sFP.P;
    if (hit) {for (int i=0;i<3;i++) {dHit[i]=hit->x()[i];};X = dHit;}
    double dS = myHlx.Path(X); myHlx.Move(dS,Fhlx);
    totLen+=dS;
    StvNodePars hFP; hFP.set(&myHlx,sFP._hz);
    StvFitErrs  sFE = node->GetFE(2);
    StvFitErrs  hFE; hFE.Set(&myHlx,sFP._hz);


    if (hit) {//Hit is there. Calculate Xi2i etc...
      const float  *hix = hit->x();
      StvNodePars iFP(hFP); iFP._x=hix[0];iFP._y=hix[1];iFP._z=hix[2];
      StvFitPars  fp = hFP-iFP;

      const double *hRR = node->GetHE();
      myXi2 = fp.mH /(hFE.mHH+hRR[0]) *fp.mH;
      myXi2+= fp.mZ /(hFE.mZZ+hRR[2]) *fp.mZ;

      if (mWorstXi2 < myXi2) { mWorstXi2 = myXi2; mWorstNode = node;}
      tstXi2 +=  myXi2;

// 	Node is ready. What to do with?    
      switch(mode&(4|8)) {
        case 4: 
         printf("HelixPars(%g) Xi2i=%g ",totLen,         myXi2); hFP.print();
         hFE.Print("HelixErrs");
         break;

        case 4|8: 
         printf("HelixPars(%g) Xi2i=%g ",totLen,         myXi2); hFP.print();
         printf("StvixPars(%g) Xi2i=%g ",totLen,node->GetXi2()); sFP.print();
         hFE.Print("HelixErrs");
         sFE.Print("StvixErrs");
         break;
      }

    }//end if hit loop

    if (!(mode&2))	continue; //No update
    node->mLen = totLen;
    node->SetPre(hFP,hFE,0);node->SetPre(hFP,hFE,1);
    node->SetFit(hFP,hFE,0);node->SetFit(hFP,hFE,1);
    if (preNode) {hFP.convert(Fstv,Fhlx); node->SetDer(Fstv,1);}    
    if (!hit) 		continue;
    node->SetXi2(myXi2,0);node->SetXi2(myXi2,1);
  }//end of 2nd loop
  tstXi2/=hlx.Ndf();
  double qwe = mXi2;
//??  assert(fabs(tstXi2-mXi2)< 0.1*(tstXi2+mXi2));


  return 0;
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Check(StvTrack *trak)
{
static int nCall = 0; nCall++;
   if (!StvDebug::Level()) 		return 0;
   if (!StvDebug::Flag("BigPt"))	return 0;
   if (trak->size()<10)			return 0;
   Helix(trak,1);  
   StvNode *node = trak->GetNode(StvTrack::kFirstPoint);
// StvNode *node = trak->front();
   double s = mHelx->Path(node->GetFP().P);
   mHelx->Move(s);
   const StvFitErrs &fe = node->GetFE();
   StvFitErrs my;
   my.Set(mHelx,fe.mHz);


#define OLEG(a,b) (2*fabs(a-b)/(fabs(a)+fabs(b)+1e-11))
  int ierr = 0;
  for (int i=0,li=0;i< 5;li+=++i) {
    if (OLEG(my[li+i],fe[li+i])<0.3) continue;
     ierr = ierr*10+i+1;
     printf(" Err.%d = %g != %g\n",i,fe[li+i],my[li+i]);
  };
//  assert(!ierr);
  return ierr;
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Check(const StvNodePars &parA,const StvFitErrs &errA,
				const StvNodePars &parB,const StvFitErrs &errB)
{
  if (!StvDebug::Level()) 	return 0;
  if (!StvDebug::Flag("BigPt")) return 0;

  THelixTrack helx;
  parA.get(&helx);  
  errA.Get(&helx);  
  double s = helx.Path(parB.P);  
  helx.Move(s);
  StvFitErrs my;
  my.Set(&helx,errB.mHz);
  int ierr = 0;
  for (int i=0,li=0;i< 5;li+=++i) {
    if (OLEG(my[li+i],errB[li+i])<0.1) continue;
    ierr = ierr*10+i+1;
     printf(" Err.%d = %g != %g\n",i,errB[li+i],my[li+i]);
  };
  int rxy = parB.getRxy();
  printf("%3d Propagate HHold=%g HHnow=%g(%g) len=%g\n",rxy,errA.mHH,errB.mHH,my.mHH,s);
  printf("              ZZold=%g ZZnow=%g(%g)       \n",    errA.mZZ,errB.mZZ,my.mZZ  );

//  assert(!ierr);
  return ierr;
}
   
   
   
