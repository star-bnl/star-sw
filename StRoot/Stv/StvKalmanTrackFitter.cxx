#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"

#include "TCernLib.h"
#include "TSystem.h"

#define _THelixNew_
#include "StarRoot/TRungeKutta.h"
#include "StvKalmanTrackFitter.h"
#include "Stv/StvToolkit.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvELossTrak.h"
#include "Stv/StvFitter.h"
#include "Stv/StvEnum.h"
#include "Stv/StvConst.h"
#include "Stv/StvStl.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
ClassImp(StvKalmanTrackFitter)


#define DOT(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define SUB(a,b,c) {c[0]=a[0]-b[0];c[1]=a[1]-b[1];c[2]=a[2]-b[2];}
#define DIST2(a,b) ((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])+(a[2]-b[2])*(a[2]-b[2]))
#define OLEG(a,b) (2*fabs(a-b)/(fabs(a)+fabs(b)+1e-11))

static const int    kXtendFactor  = 10;//Xi2 factor that fit sure failed
static const double kPiMass=0.13956995;
static const double kMinP = 0.01,kMinE = sqrt(kMinP*kMinP+kPiMass*kPiMass);
//static const double kMaxCorr = 0.1;

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
void StvKalmanTrackFitter::SetCons(const StvKonst_st *kons)
{
  mKons = kons;
}

//_____________________________________________________________________________
int StvKalmanTrackFitter::Refit(StvTrack *trak,int dir, int lane, int mode)
{
///	refit or smouthe track, using the previous Kalman.
///     dir=0 moving from out to in
///     lane 
///     dir=1 moving from in to out 
///     mode=0 No join
///     mode=1 Join
///     fit direction is imagined from left to rite
static int nCall=0; nCall++;

static StvFitter *fitt = StvFitter::Inst();

//term	LEFT here: Curren Kalman chain of fits from left to rite
//	Rite here: Previous Kalman chain, allready fitted, in different direction.
//	It was made from rite to left

  int nErr = 0;

  StvNode *node=0,*preNode=0;
  int iNode=0,iFailed=0;
  double myXi2=3e33;
//		Moving out of beam
  StvNodeIter it,itBeg,itEnd; 
  for (int iDir = dir, nDir = 0; nDir<mode; iDir = 1-iDir,nDir++) {
    if (iDir) { //fit in ==> outiDir = 1-iDi
      itBeg = trak->begin();        itEnd = trak->end();
    } else   {//fit out ==> in
      itBeg = trak->end(); --itBeg; itEnd = trak->begin();--itEnd;
    }
    node=0,preNode=0,iNode=0;
    for (it=itBeg; it!=itEnd; (iDir)? ++it:--it) {//Main loop
      preNode=node;
      node = *it; iNode++;
      const StvHit *hit = node->GetHit();
      if (!preNode)  {	// Empty leading node
//		It was not fits before(left) but shoulf be now. 
//		get params from previous dir and set huge errors
	node->SetFit(node->mFP[2],node->mFE[2],lane); 
	assert(node->mFE[lane][0]>0);
	node->mFE[lane].Recov();		//But not too big
	node->mFE[lane]*=kKalmanErrFact;	//Increase  errors
        node->mPP[lane] = node->mFP[lane];
        node->mPE[lane] = node->mFE[lane];
      } else {
  //		It was fits before. Propagate it
	int ierr = Propagate(node,preNode,iDir,lane);		//prediction from last fit
	if (ierr) return 2;
      }  
      if (!hit) { 	// no hit , fitted = predicted 
	node->SetFit(node->mPP[lane],node->mPE[lane],lane); 
      } else { // fit now  
	assert(node->mPE[lane][2]>0);
	fitt->Set(node->mPP+lane,node->mPE+lane,node->mFP+lane,node->mFE+lane);
	fitt->Prep();
	myXi2 = fitt->Xi2(hit); 
  //    =================================================
	node->SetXi2(myXi2,lane);
	if (myXi2> mKons->mXi2Hit) {//Xi2 is too big but not too big. May be will improv later
          if ( myXi2> mKons->mXi2Hit*kXtendFactor) { // Xi2 is huge. Hit not accepted
             node->SetHit(0); hit = 0; 
          }
          myXi2 = 3e3;
          node->SetFit(node->mPP[lane],node->mPE[lane],lane); 
	} else {
	  iFailed  = fitt->Update(); if (iFailed) nErr+=100;		
          node->mFP[2] = node->mFP[lane];
          node->mFE[2] = node->mFE[lane];
	}
	node->SetXi2(myXi2,lane);
if (hit) printf(" Xi2aaa %g  #%d\n",myXi2,iNode);
      }
    }	// end of nodes 
    lane = 1 - lane; 
    
  } // end of idir


   
  iNode = 0;
  itBeg = trak->begin(); itEnd = trak->end();
  for (it=itBeg; it!=itEnd; ++it) {//Main loop
      node = *it; iNode++;
      if (iNode==1) { //Beginneng no prediction in second lane
        node->SetFit(node->mFP[0],node->mFE[0],2); 
        node->SetXi2(node->GetXi2(0),2);
      } else {  
        const StvHit *hit = node->GetHit();
        fitt->Set(node->mFP+0,node->mFE+0
                 ,node->mPP+1,node->mPE+1
                 ,node->mFP+2,node->mFE+2);
        node->SetXi2(3e33,2);
        myXi2 = fitt->Xi2(); iFailed = fitt->IsFailed();
        fitt->Update();        

//    =============================================
        myXi2*= 2./5;
        if (hit) myXi2 = fitt->Xi2Join(hit);
        node->SetXi2(myXi2,2);
if (hit) printf(" Xi2bbb %g  #%d\n",myXi2,iNode);
      }
  }//endJoinoop

  return 0;
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::RefitLoop(StvTrack *tk, int idir, int ilane,int numb)
{
static const double kEps = 1.e-2;

  int ans=0,lane = ilane,state=0,dir=idir;
  int& nHits = NHits();
  nHits = tk->GetNHits();
  StvNode *tstNode = tk->GetNode(StvTrack::kLastPoint);
  StvNodePars lstPars(tstNode->GetFP());	//Remeber params to compare after refit	
  int nIters = 0,converged = 0;
  for (int refIt=0; refIt<20; refIt++)  	{	//Fit iters
    nIters++;
    ans = Refit(tk,dir,lane,numb);
//    ==================================
    nHits=NHits();
    dir = 0; lane = 0; numb = 2;
    if (nHits < mKons->mMinHits) break;
//???      if (ans>0) break;			//Very bad

    double dif = lstPars.diff(tstNode->GetFP(),tstNode->GetFE());
printf("DIFF = %g  %d\n",dif,nIters);
    if ( dif < kEps) {//Fit converged
      converged = 1; break;  
    }
    lstPars = tstNode->mFP[2]; 
  }// End Fit iters


  state = (ans>0) 
	+ 10*((!converged) 
	+ 10*((nHits < mKons->mMinHits)));
  return state;

}

//_____________________________________________________________________________
int StvKalmanTrackFitter::Propagate(StvNode  *node,StvNode *preNode,int dir,int lane)
{
  StvNode *innNode=0,*outNode=0;  if (innNode){}; if (outNode){};
  if (!dir) {innNode = node; outNode=preNode;}
  else      {outNode = node; innNode=preNode;}

  TRungeKutta myHlx;
  myHlx.SetDerOn();
  const StvNodePars &prePars =  preNode->mFP[lane];
  const StvFitErrs  &preErrs =  preNode->mFE[lane];
  prePars.get(&myHlx);
  preErrs.Get(&myHlx);
  double Xnode[3];
  if (node->mHit) 	{ TCL::ucopy(node->mHit->x(),Xnode,3);}
  else        		{ TCL::ucopy(node->mXDive   ,Xnode,3);}
//   	double dis = sqrt(DIST2(Xnode,preNode->mFP[lane]._x));
//   	if (!dir) dis = -dis;
//   	myHlx.Move(dis);
  double dS = myHlx.Path(Xnode);		
assert(fabs(dS)<1e3);
  myHlx.Move(dS);
  node->mPP[lane].set(&myHlx);
  node->mPE[lane].Set(&myHlx);
  StvELossTrak *eloss = innNode->ResetELoss(prePars,dir);
  node->mPP[lane].add(eloss,dS);
  node->mPE[lane].Add(eloss,dS);

  node->mPE[lane].Recov();

  return 0;
  
}

//_____________________________________________________________________________
int StvKalmanTrackFitter::Fit(const StvTrack *trak,const StvHit *vtx,StvNode *node)
{
/// function for track fitting to primary vertex

static int nCall = 0; nCall++;
static StvToolkit *kit  = StvToolkit::Inst(); if(kit){}
static StvFitter  *fitt = StvFitter::Inst();
enum {kDeltaZ = 100};//??????

  const StvNode *lastNode = trak->GetNode(StvTrack::kDcaPoint);
  if (!lastNode) return 1;
  if (fabs(vtx->x()[2]-lastNode->GetFP().getZ()) > kDeltaZ) return 2;
  TRungeKutta th;
  lastNode->GetFP().get(&th);
  lastNode->GetFE().Get(&th);
  const float *h = vtx->x();
  double d[3]={h[0],h[1],h[2]};
  double len = th.Path(d);
  double x[3];
  th.Eval(len,x);
  mDca3 = DIST2(d,x);
  th.Move(len);
  StvNodePars par[2]; par[0].set(&th);
  StvFitErrs  err[2]; err[0].Set(&th);
  fitt->Set(par+0,err+0,par+1,err+1);
  fitt->Prep();
  mXi2 = fitt->Xi2(vtx);
  if (!node) return 0;
  
  fitt->Update();
  assert(err[1][0]>0);
  assert(err[1][2]>0);


  mXi2 = fitt->GetXi2();
  node->SetPre(par[0],err[0],0);
  node->SetFit(par[1],err[1],0);
//  assert(th.Der());///???
  if (th.Der()) {
    StvFitDers fiDers(*th.Der());
    node->SetDer(fiDers,0);
  }
  return 0;
}   
//_____________________________________________________________________________
THelixTrack* StvKalmanTrackFitter::GetHelix() const {return mHelx;}

//_____________________________________________________________________________
int StvKalmanTrackFitter::Helix(StvTrack *trak,int mode)
{
static int nCall=0;nCall++;
enum {kUseErrs=1, kUpdate=2, kPrint=4};
// mode &1 use err
// mode &2 = update track
// mode &4 = print

  if (!mode         ) mode = kPrint;
  mXi2 = 0;
  if (!mHelx) mHelx = new THelixFitter;
  mHelx->Clear();
  THelixFitter& hlx = *mHelx;
  StvNode *node=0,*preNode=0; if (preNode){};
  for (StvNodeIter it=trak->begin();it!=trak->end(); ++it) {
    node = *it; 
    const StvHit *hit= node->GetHit();
    if (!hit) continue;
    hlx.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
    if(mode&kUseErrs) 		{	//Account errors
      double cosL = node->GetFP().getCosL(); 
      const double *rr = node->GetHE();    
      assert(rr[0]>0);assert(rr[2]>0);assert(rr[0]*rr[2]>rr[1]*rr[1]);
      hlx.AddErr(rr[0],rr[2]/(cosL*cosL));
    }
  }  
  mXi2 = 3e33; if (hlx.Used()<3) return 1;
  mXi2 =hlx.Fit();
  if(mode&kUseErrs) { hlx.MakeErrs();}
  double dL = hlx.Path(trak->front()->GetFP().pos());
  hlx.Move(dL);
  if ((mode&(kUpdate|kPrint))==0) return 0;
  node=0;
  assert(hlx.Emx());
  double dHit[3],tstXi2=0,myXi2=0;

//		Loop for Print,compare & update
  double totLen=0;
  TRungeKutta myHlx(hlx);
  int iNode = -1;
  for (StvNodeIter it=trak->begin();it!=trak->end(); ++it) {
    iNode++;preNode=node; node = *it;
    const StvHit *hit = node->GetHit();
    const float  *hix = (hit)? hit->x():0;

    const double *X = node->mXDive;
    if (hit) {for (int i=0;i<3;i++) {dHit[i]=hix[i];};X = dHit;}
    double dS = myHlx.Path(X); myHlx.Move(dS);
    totLen+=dS;
    StvNodePars hFP; hFP.set(&myHlx);
    StvFitErrs  hFE; hFE.Set(&myHlx);


    myXi2 = 6e6;
    if (hix) {//Hit is there. Calculate Xi2i etc...
      StvNodePars iFP; TCL::ucopy(hix,iFP.pos(),3);
      StvFitPars  fp = hFP-iFP;

      const double *hRR = node->GetHE();
      myXi2 = fp[0] /(hFE[0]+hRR[0]) *fp[0];
      myXi2+= fp[1] /(hFE[2]+hRR[2]) *fp[1];

       tstXi2 +=  myXi2;
    }

    if (mode&kUpdate)	{ 		//Update
      node->mLen = totLen;
      node->SetPre(hFP,hFE,0);
      node->SetFit(hFP,hFE,0);
      node->SetXi2(myXi2,0);
    }

    if (mode&kPrint)	{ 		//Print Helix
      printf("HelixPars(%g) Xi2i=%g ",totLen,myXi2); hFP.print();
      if (mode&1) hFE.Print("HelixErrs");
    }  
    
  }//end of hit loop

  tstXi2/=hlx.Ndf();if (tstXi2){};
  double qwe = mXi2; if (qwe){};//only to see it in gdb
  return 0;
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Check(StvTrack *trak)
{
static int nCall = 0; nCall++;
   if (trak->size()<10)			return 0;
   Helix(trak,1);  
   StvNode *node = trak->GetNode(StvTrack::kFirstPoint);
// StvNode *node = trak->front();
   double s = mHelx->Path(node->GetFP());
   mHelx->Move(s);
   const StvFitErrs &fe = node->GetFE();
   StvFitErrs my;
   my.Set(mHelx);


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
  TRungeKutta helx;
  parA.get(&helx);  
  errA.Get(&helx);  
  double s = helx.Path(parB);  
  helx.Move(s);
  StvFitErrs my;
  my.Set(&helx);
  int ierr = 0;
  for (int i=0,li=0;i< 5;li+=++i) {
    if (OLEG(my[li+i],errB[li+i])<0.1) continue;
    ierr = ierr*10+i+1;
     printf(" Err.%d = %g != %g\n",i,errB[li+i],my[li+i]);
  };
  int rxy = parB.getRxy();
  printf("%3d Propagate HHold=%g HHnow=%g(%g) len=%g\n",rxy,errA[0],errB[0],my[0],s);
  printf("              ZZold=%g ZZnow=%g(%g)       \n",    errA[2],errB[2],my[2]  );

//  assert(!ierr);
  return ierr;
}

