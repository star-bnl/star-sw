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

static const double kMaxCorr = 1.1;
static const double kMinPti = 0.05;	//min value 1/pt allowed
static const int    kNodesEnough = 3;	//min number of nodes to define helix
static const int    kXtendFactor  = 9;	//Xi2 factor that fit sure failed

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
int StvKalmanTrackFitter::Refit(StvTrack *trak,int dir, int lane, int mode)
{
///	refit or smouthe track, using the previous Kalman.
///     dir=0 moving from out to in
///     dir=1 moving from in to out 
///     mode=0 No join
///     mode=1 Join
///     fit direction is imagined from left to rite
static int nCall=0; nCall++;

static StvFitter *fitt = StvFitter::Inst();
static const StvKonst_st  *kons = StvConst::Inst();
  StvNode *breakNode = 0;

//term	LEFT here: Curren Kalman chain of fits from left to rite
//	Rite here: Previous Kalman chain, allready fitted, in different direction.
//	It was made from rite to left

  int jane=1-lane;
  int nFitLeft=0;		// number of fits made by this pass excluding current node
  int nFitRite=0;		// number of fits made during previous
  				// pass in different direction, including current node 
  int wasFitted=0;		// this node was fitted in previous pass
  if (mode&1) {//Join of two lanes
    nFitRite = trak->GetNFits(jane);}
  else        {//no join. lanes independent
    jane=lane;
  }
  int nErr = 0;

  StvNodeIter it,itBeg,itEnd;
  if (dir) { //fit in ==> out
    itBeg = trak->begin();        itEnd = trak->end();
  } else   {//fit out ==> in
    itBeg = trak->end(); --itBeg; itEnd = trak->begin();--itEnd;
  }


  double myXi2=3e33;
  mWorstXi2=0;mWorstNode=0;
  StvNode *node=0,*preNode=0,*innNode=0,*outNode=0;
  int iNode=0;

  for (it=itBeg; it!=itEnd; (dir)? ++it:--it) {//Main loop
    preNode=node;
    node = *it; iNode++;
    if (!dir) 	{ innNode = node; outNode = preNode;}
    else 	{ outNode = node; innNode = preNode;}
enum myCase {kNull=0,kLeft=1,kRite=2,kHit=4,kFit=8  };
    node->GetFE(lane).mHH = -1;
    node->GetFE(lane).mHH = -1;

//         if (node->GetType()==StvNode::kDcaNode) {
//           printf("DCAInit lane=%d err=%g\n",jane,sqrt(node->mFE[jane].mPP));
//           assert(!(nFitLeft*nFitRite));
//         }

    int kase = 0;
    if (nFitLeft) 		kase|=kLeft;
    if (mode && nFitRite) 	kase|=kRite;
    const StvHit *hit = node->GetHit();
    wasFitted = (mode&1)? node->IsFitted(jane):0;
//     if (wasFitted ) 		kase|=kHit;
    if (hit)			kase|=kHit;

// Imaginary we are always coming from left to right.
// kLeft 		= left fits only, no hit
// kLeft+kHit 		= Left fits only, now hit, 
// kRite 		= No left, no hit, rite fits only
// kLeft+kRite 		= left fits, no hit, rite fits
// kHit+kRite 		= No left fits, now hit, rite fits
// kLeft+kRite+kHit 	= Left fits,now hit, rite fits
    node->SetXi2(3e33,lane);
    switch (kase) {// 1st switch, fill PREDICTION

      default: assert(0 && "Wrong Case1");

      case kRite|kHit: 	// No left, now Hit, rite fits 
      case kNull|kHit: 	// No left, now Hit, No rite  
      case kRite: 	// No left, no  Hit, rite fits 
      case kNull: {	// Empty leading node
//		It was not fits before(left) get params from previous dir
//		and set huge errors
        node->mPP[lane] = node->mFP[2];		//prediction from opposite fit
        node->mPE[lane] = node->mFP[2].deltaErrs();	//Big errors
        break;
      }  
      case kLeft: 		// Left fits only, no  Hit
      case kLeft|      kHit: 	// Left fits only, now Hit
      case kLeft|kRite     : 	// Left fits, no Hit, Rite fits
      case kLeft|kRite|kHit: 	// Left fits,now Hit, Rite fits
      {
//		It was fits before. Propagate it
       Propagate(node,preNode,dir,lane);		//prediction from last fit
       break;
      }  

    }//End 1st prediction switch

//         if (node->GetType()==StvNode::kDcaNode) {
//           printf("DCA1st lane=%d err=%g\n",lane,sqrt(node->mPE[lane].mPP));
//         }
    switch (kase) {// 2nd switch, fill FITD

      case kNull: 	// No   fits  no  Hit
      case kLeft: 	// Left fits only, no  Hit
      case kRite: 	// Rite fits only, no  Hit
      case kLeft|kRite: // Left fits, no Hit, Rite fits
      {
//		No hit. Fit = Prediction		
        node->SetFit(node->mPP[lane],node->mPE[lane],lane); 
        break;
      }

      case kNull|      kHit: // Left fits only, now Hit
      case kLeft|      kHit: // Left fits only, now Hit
      case kRite|      kHit: // No left, now Hit, rite fits 
      case kLeft|kRite|kHit: // Left fits,now Hit, Rite fits
      {
//		Fit it		
	fitt->Set(node->mPP+lane,node->mPE+lane,node->mFP+lane,node->mFE+lane);
	fitt->Prep();

	myXi2 = fitt->Xi2(hit);
//      =======================
	if (myXi2> kons->mXi2Hit) 	{ //Fit is bad yet
	  nErr++;		
          if (myXi2> kons->mXi2Hit*kXtendFactor) { // Fit failed. Hit not accepted
             node->SetHit(0);
//		No hit anymore. Fit = Prediction		
             node->SetFit(node->mPP[lane],node->mPE[lane],lane); 
             break;}
        }
        if (myXi2>mWorstXi2) 		{ mWorstXi2= myXi2; 		mWorstNode=node;}
        if (node->mXi2[jane]>mWorstXi2) { mWorstXi2= node->mXi2[jane]; 	mWorstNode=node;}
        nFitLeft++; kase|=kLeft;
	int ipdate = fitt->Update();
        if (ipdate) nErr++;		//Update failed

	node->SetXi2(myXi2,lane);
        if (nFitLeft<=kNodesEnough && jane!=lane) {
	  node->mFP[lane] = node->mFP[jane];}

        node->mFP[2] = node->mFP[lane];
        node->mFE[2] = node->mFE[lane];

        break;
      }

      default: assert(0 && "Wrong Case2");

    }//end 2nd switch
//         if (node->GetType()==StvNode::kDcaNode) {
//           printf("DCA2nd lane=%d err=%g\n",lane,sqrt(node->mFE[lane].mPP));
//         }

    if (!mode) continue;

    switch (kase) {// 3rd switch,JOIN

       case kNull: 	// No   fits  no  Hit
       case kRite: 	// Rite fits only, no  Hit
//		No hit. No own ifo yet. Get everything from opposite fit		
       {
        node->SetFit(node->mFP[jane],node->mFE[jane],2); 
        break;
       }

       case kLeft: 	// Left fits only, no  Hit
//		No hit. No own ifo yet. Get everything from opposite fit		
       {
        node->SetFit(node->mFP[lane],node->mFE[lane],2); 
        break;
       }

       default:
      {
	breakNode = (dir)? node:preNode;
	fitt->Set(node->mFP+lane         ,node->mFE+lane
        	 ,node->mPP+jane         ,node->mPE+jane
        	 ,node->mFP+2            ,node->mFE+2   );
	node->SetXi2(3e33,2);
	myXi2 = fitt->Xi2(); 

	if (myXi2 > kons->mXi2Joi*kXtendFactor) return -13;
	if (myXi2 > kons->mXi2Joi) nErr+=100;
	int ipdate = fitt->Update();
        if (ipdate) {//Update error 
	  myXi2= 3e13;nErr+=1000;
        }
	node->SetXi2(myXi2,2);
        break;
      }

    }//End 3rd case
//         if (node->GetType()==StvNode::kDcaNode) {
//           printf("DCA3rd lane=%d err=%g\n",2,sqrt(node->mFE[2].mPP));
//         }

    nFitRite-= wasFitted;

  }//endMainLoop

  return nErr;
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Propagate(StvNode  *node,StvNode *preNode,int dir,int lane)
{
static int nCall=0; nCall++;
  StvNode *innNode=0,*outNode=0;
  if (!dir) {innNode = node; outNode=preNode;}
  else      {outNode = node; innNode=preNode;}
//????  assert(innNode->GetFP().getRxy()<outNode->GetFP().getRxy());

#if 0 //???????????????????????????????????????????????
  do { //Attempt to propagate with linear approximation;
    node->mPP[lane] = node->mFP[1-lane];
    StvFitPars delPre = preNode->mFP[lane]-preNode->mPP[1-lane];
    if (delPre.TooBig(preNode->mFP[lane])) 	break;
    StvFitPars del    = delPre*innNode->mDer[lane];
    if (del.TooBig(node->mPP[lane]))    		break; 
    node->mPP[lane]+= del;node->mFP[lane]=node->mPP[lane];
//        node->mPP[lane]+= ((preNode->mFP[lane]-preNode->mPP[1-lane])*node->mDer[lane]);
    node->mPE[lane] = preNode->mFE[lane]*node->mDer[lane];
    node->mPE[lane].SetHz(node->mPP[lane]._hz);
    node->mPE[lane].Add(innNode->mELossData,node->mPP[lane]);
    if(node->mPE[lane].Check()) break; 
//    return 0;
  } while(0);
#endif

//		Propagate with THelixTrack

  THelixTrack myHlx;
  preNode->mFP[lane].get(&myHlx);
  preNode->mFE[lane].Get(&myHlx);
  const double *P = node->mFP[0].P;
  double dS = myHlx.Path(P);
  StvHlxDers derHlx;
  StvFitDers derFit;
  double rho = myHlx.GetRho();
  const StvELossData &el = innNode->GetELoss();
  double dRho = rho*(el.mdPP/el.mTotLen)*dS;
//assert(dRho/rho*(2*dir-1)>=0);
  myHlx.Set(rho+dRho/3);
  dS = myHlx.Path(P);
  myHlx.Move(dS,derHlx);
  myHlx.Set(rho+dRho);
  node->mPP[lane].set(&myHlx,node->GetHz());
  node->mPE[lane].Set(&myHlx,node->GetHz());
  node->mPE[lane].Add(innNode->mELossData,node->mPP[lane]);
  node->mPP[lane].convert(derFit,derHlx);
  innNode->SetDer(derFit,lane);

  return 0;
  
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Fit(const StvTrack *trak,const StvHit *vtx,StvNode *node)
{
static       StvToolkit *kit     = StvToolkit::Inst();
static const StvKonst_st *myConst =   StvConst::Inst();
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
  StvFitDers derivFit;
  StvHlxDers derivHlx;
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
  node->SetDer(derivFit,0);
  return 0;
}   
//_____________________________________________________________________________
THelixTrack* StvKalmanTrackFitter::GetHelix() const {return mHelx;}

//_____________________________________________________________________________
int StvKalmanTrackFitter::Helix(StvTrack *trak,int mode)
{
static int nCall=0;nCall++;
// mode &1 use err
// mode &2 = update track
// mode &4 = print
// mode &8 = print+compare
// mode &16 = histogramm

  if (!mode  ) mode = 4;
  if ( mode&8) mode|= 4;
  mWorstXi2=0,mWorstNode = 0;mXi2 = 0;
  if (!mHelx) mHelx = new THelixFitter;
  mHelx->Clear();
  StvFitDers Fstv;
  StvHlxDers Fhlx;
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
  if(mode&1) { hlx.MakeErrs();}
  double dL = hlx.Path(trak->front()->GetFP().P);
  hlx.Move(dL);

  node=0;
  double dHit[3],tstXi2=0,myXi2=0;

//		Loop for Print,compare & update
  double totLen=0;
  THelixTrack myHlx(hlx);
  int iNode = -1,nHist=0;
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
    }

    if (mode&2)	{ 		//Update
      node->mLen = totLen;
      node->SetPre(hFP,hFE,0);node->SetPre(hFP,hFE,1);
      node->SetFit(hFP,hFE,0);node->SetFit(hFP,hFE,1);
      if (preNode) {hFP.convert(Fstv,Fhlx); preNode->SetDer(Fstv,1);}    
      if (!hit) node->SetXi2(myXi2,0);node->SetXi2(myXi2,1);
    }

    if (mode&4)	{ 		//Print Helix
      printf("HelixPars(%g) Xi2i=%g ",totLen,myXi2); hFP.print();
      if (mode&1) hFE.Print("HelixErrs");
    }  

    if (mode&8)	{ 		//Print StvFit
      printf("StvixPars(%g) Xi2i=%g ",totLen,node->GetXi2()); sFP.print();
      sFE.Print("StvixErrs");
    }
    if (mode&16 &&  hit && !nHist){ 	//Histogram
static const char *fiNam[]={"H","Z","A","L","P"};
      nHist++;
      for (int i=0,li=0;i< 5;li+=++i) {
        TString ts("FiterrByHelix"); ts +=fiNam[i];
	StvDebug::Count(ts,sFE.Arr()[li+i]/hFE.Arr()[li+i]);
    } }
    if (mode&16 && node->GetType()==StvNode::kDcaNode){ 	//Dca Histogram
static const char *fiNam[]={"H","Z","A","L","P"};
      for (int i=0,li=0;i< 5;li+=++i) {
        TString ts("FitDcaByHelix"); ts +=fiNam[i];
	StvDebug::Count(ts,sFE.Arr()[li+i]/hFE.Arr()[li+i]);
    } }
    
  }//end of hit loop

  tstXi2/=hlx.Ndf();if (tstXi2){};
  double qwe = mXi2; if (qwe){};//only to see it in gdb
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
   
   
   
