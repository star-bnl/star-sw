#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"

#include "TCernLib.h"
#include "TSystem.h"
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
#include "Stv/StvConst.h"
ClassImp(StvKalmanTrackFitter)
#define DIST2(a,b) ((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])+(a[2]-b[2])*(a[2]-b[2]))

static const int    kXtendFactor  = 10;//Xi2 factor that fit sure failed
static const double kPiMass=0.13956995;
static const double kMinP = 0.01,kMinE = sqrt(kMinP*kMinP+kPiMass*kPiMass);
static const double kMaxCorr = 0.1;
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
///     dir=1 moving from in to out 
///     mode=0 No join
///     mode=1 Join
///     fit direction is imagined from left to rite
static int nCall=0; nCall++;

static StvFitter *fitt = StvFitter::Inst();

//term	LEFT here: Curren Kalman chain of fits from left to rite
//	Rite here: Previous Kalman chain, allready fitted, in different direction.
//	It was made from rite to left

  int jane=1-lane;
  int nFitLeft=0;		// number of fits made by this pass excluding current node
  int nFitRite=0;		// number of fits made during previous
  				// pass in different direction, including current node 
  int wasFitted=0;		// this node was fitted in previous pass
  mNHits = trak->GetNFits(jane);
  if (mode&1) {//Join of two lanes
    nFitRite = mNHits;}
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
  StvNode *node=0,*preNode=0,*innNode=0,*outNode=0;
  if(innNode){}; if(outNode){};
  int iNode=0,iFailed=0;

  for (it=itBeg; it!=itEnd; (dir)? ++it:--it) {//Main loop
    preNode=node;
    node = *it; iNode++;
    if (!dir) 	{ innNode = node; outNode = preNode;}
    else 	{ outNode = node; innNode = preNode;}
enum myCase {kNull=0,kLeft=1,kRite=2,kHit=4,kFit=8  };
    node->GetFE(lane).mHH = -1;
    node->GetFE(lane).mZZ = -1;

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
      {	// Empty leading node
//		It was not fits before(left) but shoulf be now. 
//		get params from previous dir and set huge errors
        node->mPP[lane] = node->mFP[2];		//prediction from opposite fit
        node->mPE[lane] = node->mFE[2];	
        assert(node->mPE[lane].mHH>0);
        assert(node->mPE[lane].mZZ>0);

	node->mPE[lane]*=kKalmanErrFact;	//Big errors
        node->mPE[lane].Recov();		//But not too big
        break;
      }  

      case kRite: 	// No left, no  Hit, rite fits 
      case kNull: 
      {	// Empty leading node
//		It was not fits before(left) get params from previous dir
//		and set huge errors
        node->mPP[lane] = node->mFP[2];				//prediction from opposite fit
        node->mPE[lane] = node->mFE[2];				//Big errors
        assert(node->mPE[lane].mHH>0);
        assert(node->mPE[lane].mZZ>0);
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
        assert(node->mPE[lane].mHH>0);
        assert(node->mPE[lane].mZZ>0);
        node->SetFit(node->mPP[lane],node->mPE[lane],lane); 
        break;
      }

      case kNull|      kHit: // Left fits only, now Hit
      case kLeft|      kHit: // Left fits only, now Hit
      case kRite|      kHit: // No left, now Hit, rite fits 
      case kLeft|kRite|kHit: // Left fits,now Hit, Rite fits
      {
//		Fit it		
static int nQQQQ=0; nQQQQ++;
StvDebug::Break(nQQQQ);//???????????
        assert(node->mPE[lane].mHH>0);
        assert(node->mPE[lane].mZZ>0);
	fitt->Set(node->mPP+lane,node->mPE+lane,node->mFP+lane,node->mFE+lane);
	fitt->Prep();

	myXi2 = fitt->Xi2(hit); iFailed = fitt->IsFailed();
//      =================================================
	node->SetXi2(myXi2,lane);
        if (iFailed == StvFitter::kBigErrs) iFailed = 0;
	if (iFailed ) nErr+=1; 			//Fit is bad yet
	if (myXi2> mKons->mXi2Hit) nErr+=10; //Fit is bad yet
        if ( myXi2> mKons->mXi2Hit*kXtendFactor) { // Fit failed. Hit not accepted
            if (--mNHits <3) 			return 1;
            node->SetHit(0); hit = 0; nFitLeft--;
//		No hit anymore. Fit = Prediction		
        assert(node->mPE[lane].mHH>0);
        assert(node->mPE[lane].mZZ>0);
            node->SetFit(node->mPP[lane],node->mPE[lane],lane); 
            break;
	}
	iFailed  = fitt->Update(); if (iFailed) nErr+=100;		
        nFitLeft++; kase|=kLeft;
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
        assert(node->mFE[jane].mHH>0);
        assert(node->mFE[jane].mZZ>0);
        node->SetFit(node->mFP[jane],node->mFE[jane],2); 
        break;
       }

       case kLeft: 	// Left fits only, no  Hit
//		No hit. No own info yet. Get everything from opposite fit		
       {
        node->SetFit(node->mFP[lane],node->mFE[lane],2); 
        break;
       }

       default:
      {
        assert(node->mPE[lane].mHH>0);
        assert(node->mPE[lane].mZZ>0);
        assert(node->mPE[jane].mHH>0);
        assert(node->mPE[jane].mZZ>0);
	fitt->Set(node->mPP+lane         ,node->mPE+lane
        	 ,node->mPP+jane         ,node->mPE+jane
        	 ,node->mFP+2            ,node->mFE+2   );
	node->SetXi2(3e33,2);
	myXi2 = fitt->Xi2(); iFailed = fitt->IsFailed();
//      ==============================================
	node->SetXi2(myXi2/5*2,3);
	if (iFailed 		 ) 	nErr+=1000;
	if (myXi2 > mKons->mXi2Joi) 	nErr+=10000;
	iFailed = fitt->Update();
	if (iFailed) 			nErr+=100000;
        if (myXi2 > mKons->mXi2Joi*kXtendFactor || iFailed>0) { //Joining is impossible. Stop joining
          mode = 0;	//No more joinings
          node->SetFit(node->mFP[lane],node->mFE[lane],2); 
          break;
        }
        myXi2 = fitt->GetXi2();
        if (!hit)  break;


//		Fit hit	 to join data	
        StvNodePars myPars(node->mFP[2]);
        StvFitErrs  myErrs(node->mFE[2]);
        assert(node->mFE[2].mHH>0);
        assert(node->mFE[2].mZZ>0);

	fitt->Set(&myPars,&myErrs,node->mFP+2,node->mFE+2);
	fitt->Prep();

	myXi2 = fitt->Xi2(hit); iFailed = fitt->IsFailed();
//      =================================================
        if (iFailed== StvFitter::kBigErrs) iFailed=0;
	node->SetXi2(myXi2,2);
	if (iFailed		) nErr+=1000000; //Fit is bad yet
	if (myXi2> mKons->mXi2Hit) nErr+=10000000; //Fit is bad yet
        if (myXi2> mKons->mXi2Hit*kXtendFactor) { // Fit failed. Hit not accepted
          node->SetHit(0); hit = 0; nFitLeft--; mNHits--;
//		No hit anymore. Fit = Prediction		
          node->SetFit(myPars,myErrs,2); 
          break;
	} 
	iFailed = fitt->Update(); if (iFailed) nErr+=100000000;
         break;
       }

    }//End 3rd case
//         if (node->GetType()==StvNode::kDcaNode) {
//           printf("DCA3rd lane=%d err=%g\n",2,sqrt(node->mFE[2].mPP));
//         }

    nFitRite-= wasFitted;
    assert(node->mFE[lane].mHH>0);
    assert(node->mFE[lane].mZZ>0);


  }//endMainLoop

  return -nErr;
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Refit(StvTrack *tk, int idir)
{
static int nCall=0;nCall++;
static const double kEps = 1.e-2,kEPS=1e-1;

  int ans=0,anz=0,lane = 1;
  int& nHits = NHits();
  nHits = tk->GetNHits();
  int nBegHits = nHits;
  int nRepair =(nHits-5)*0.1;
  int state = 0;
  StvNode *tstNode = (idir)? tk->front(): tk->back();
  int nIters = 0,nDrops=0;
  for (int repair=0;repair<=nRepair;repair++)  	{ 	//Repair loop
    int converged = 0;
    for (int refIt=0; refIt<10; refIt++)  	{	//Fit iters
      nIters++;
      ans = Refit(tk,idir,lane,1);
//    ==================================
      nHits=NHits();
      if (nHits < mKons->mMinHits) break;
      if (ans>0) break;			//Very bad
      
      StvNodePars lstPars(tstNode->GetFP());	//Remeber params to compare after refit	
      anz = Refit(tk,1-idir,1-lane,1); 
//        ==========================================
      nHits=NHits();
      if (nHits < mKons->mMinHits) break;
      if (anz>0) break;	

      double dif = lstPars.diff(tstNode->GetFP(),tstNode->GetFE());
      double eps = (ans || anz)? kEPS:kEps;
      if ( dif < eps) { //Fit converged
      converged = 1; break; } 

    }// End Fit iters
    
    state = (ans!=0) + 10*((anz!=0) + 10*((!converged) 
          + 10*((tk->GetXi2()>mKons->mXi2Trk)+10*(nHits < mKons->mMinHits))));
    if (!state) 		break;
    if (nHits < mKons->mMinHits) 	break;
    StvNode *badNode=tk->GetNode(StvTrack::kMaxXi2);
//    StvNode *badNode=tk->GetMaxKnnNode();
    if (!badNode) 		break;
    badNode->SetHit(0); nDrops++;
    nHits--; if (nHits < mKons->mMinHits) { state = 1000000; break;}
  }//End Repair loop


  if (ans<=0) state &= (-2);
  if (anz<=0) state &= (-4);
  
  nHits = tk->GetNHits();
  nDrops = nBegHits-nHits;
  return state;

}

//_____________________________________________________________________________
int StvKalmanTrackFitter::Propagate(StvNode  *node,StvNode *preNode,int dir,int lane)
{
static int nCall=0; nCall++;
static const char* PropagateHelix = gSystem->Getenv("PropagateHelix");
StvDebug::Break(nCall);
  StvFitDers derFit;

  StvNode *innNode=0,*outNode=0;
  if (innNode){}; if (outNode){};
  if (!dir) {innNode = node; outNode=preNode;}
  else      {outNode = node; innNode=preNode;}
  
  double saveCurv = node->mFP[lane]._curv;
  node->mFP[lane]._curv = (2*saveCurv+preNode->mFP[lane]._curv)/3;
  
double dS=0;
if (PropagateHelix) 	//Propagate with THelixTrack ????? HACK
{
  THelixTrack myHlx;
  preNode->mFP[lane].get(&myHlx);
  const StvNodePars &prePars =  preNode->mFP[lane];
  assert(preNode->mFE[lane].mHH>0);
  const StvFitErrs  &preErrs =  preNode->mFE[lane];
  prePars.get(&myHlx);
  preErrs.Get(&myHlx);
  double Xnode[3];
  if (node->mHit) 	{ TCL::ucopy(node->mHit->x(),Xnode,3);}
  else        		{ TCL::ucopy(node->mXDive   ,Xnode,3);}

  double dS = myHlx.Path(Xnode);
  StvHlxDers derHlx;
//		reset ELossData for may be new momentum
  innNode->ResetELoss(preNode->mFP[lane],dir);
  const StvELossTrak *el = innNode->GetELoss();
  double dP = 0;
  if (el) {//account energy loss
    double P = sqrt(prePars.getP2()); 
    dP = el->PLoss(P)*(-dS);
  }
  if (dP > kMaxCorr) dP = kMaxCorr;
  if (dP <-kMaxCorr) dP =-kMaxCorr;
  double rho = prePars._curv;
  double dRho = -rho*(dP);
  myHlx.Set(rho+dRho/3);
  dS = myHlx.Path(Xnode);
  myHlx.Move(dS,derHlx);
  myHlx.Set(rho+dRho);
  node->mPP[lane].set(&myHlx,node->GetHz());
  node->mPE[lane].Set(&myHlx,node->GetHz());
  node->mPE[lane].Recov();
  node->mPE[lane].Add(el,node->mPP[lane],dS);
  node->mPP[lane].convert(derFit,derHlx);
  innNode->SetDer(derFit,lane);
  
  } 
else
  {
  StvNodePars pars = preNode->mFP[lane];
  double p = pars.getP(); 
  
  const StvELossTrak *el = innNode->GetELoss();
  double dPP = 0,dPdP0=0;
  if (el) {
    innNode->ResetELoss(preNode->mFP[lane],dir);
    dPP = el->PLoss(p)*(el->TotLen())/p;
    if (dPP > kMaxCorr) dPP = kMaxCorr;
    dPP/=el->TotLen();
  }

  double Xnode[3];
  if (node->mHit) 	{ TCL::ucopy(node->mHit->x(),Xnode,3);}
  else        		{ TCL::ucopy(node->mXDive   ,Xnode,3);}
  
  dS = pars.move(Xnode,dPP,dir);
  if ((dS>0)!=(dir>0)) {
    pars = preNode->mFP[lane];
    dS = pars.move(node->mXDive,dPP,dir);
  }

//assert((dS>0)==(dir>0));
  node->mPP[lane] = pars;

  pars.Deriv(dS,derFit);
  if (el) {
    dPdP0 = el->dPLossdP0(p);
    if (fabs(dPdP0)>0) pars.Deriv(dS,dPdP0,derFit);
  }
  StvFitErrs  &nowErrs = node->mPE[lane];
  const StvFitErrs  &preErrs =  preNode->mFE[lane];
assert(preErrs.mHH>0);
assert(preErrs.mZZ>0);
  nowErrs = preErrs*derFit;
assert(nowErrs.mHH>0);
assert(preErrs.mZZ>0);
  nowErrs.Add(el,pars,dS);
assert(nowErrs.mHH>0);
assert(preErrs.mZZ>0);
  innNode->SetDer(derFit,lane);
  } 

  node->mFP[lane]._curv = saveCurv;

  return 0;
  
}
//_____________________________________________________________________________
int StvKalmanTrackFitter::Fit(const StvTrack *trak,const StvHit *vtx,StvNode *node)
{
static int nCall = 0; nCall++;
static       StvToolkit *kit     = StvToolkit::Inst();
static StvFitter *fitt = StvFitter::Inst();
enum {kDeltaZ = 100};//??????

  const StvNode *lastNode = trak->GetNode(StvTrack::kDcaPoint);
  if (!lastNode) return 1;
  if (fabs(vtx->x()[2]-lastNode->GetFP()._z) > kDeltaZ) return 2;
  THelixTrack th;
  lastNode->GetFP().get(&th);
  lastNode->GetFE().Get(&th);
  const float *h = vtx->x();
  double d[3]={h[0],h[1],h[2]};
  double len = th.Path(d);
  double x[3];
  th.Eval(len,x);
  mDca3 = DIST2(d,x);
  StvFitDers derivFit;
  StvHlxDers derivHlx;
  if (node) {th.Move(len,derivHlx);} else {th.Move(len);}
  double Hz = kit->GetHz(th.Pos());
  StvNodePars par[2]; par[0].set(&th,Hz);
  StvFitErrs  err[2]; err[0].Set(&th,Hz);
  fitt->Set(par+0,err+0,par+1,err+1);
  fitt->Prep();
  mXi2 = fitt->Xi2(vtx);
  if (!node) return 0;
  
  fitt->Update();
  assert(err[1].mHH>0);
  assert(err[1].mZZ>0);


  mXi2 = fitt->GetXi2();
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
enum {kUseErrs=1, kUpdate=2, kPrint=4};
// mode &1 use err
// mode &2 = update track
// mode &4 = print

  if (!mode         ) mode = kPrint;
  mXi2 = 0;
  if (!mHelx) mHelx = new THelixFitter;
  mHelx->Clear();
  StvFitDers Fstv;
  StvHlxDers Fhlx;
  THelixFitter& hlx = *mHelx;
  StvNode *node=0,*preNode=0; if (preNode){};
  for (StvNodeIter it=trak->begin();it!=trak->end(); ++it) {
    node = *it; 
    const StvHit *hit= node->GetHit();
    if (!hit) continue;
    hlx.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
    if(mode&kUseErrs) 		{	//Account errors
      double cos2li = node->GetFP()._tanl; cos2li = (1+cos2li*cos2li);
      const double *rr = node->GetHE();    
      assert(rr[0]>0);assert(rr[2]>0);assert(rr[0]*rr[2]>rr[1]*rr[1]);
      hlx.AddErr( rr[0],rr[2]*cos2li);
    }
  }  
  mXi2 = 3e33; if (hlx.Used()<3) return 1;
  mXi2 =hlx.Fit();
  if(mode&kUseErrs) { hlx.MakeErrs();}
  double dL = hlx.Path(trak->front()->GetFP().P);
  hlx.Move(dL);
  if ((mode&(kUpdate|kPrint))==0) return 0;
  node=0;
  double dHit[3],tstXi2=0,myXi2=0;

//		Loop for Print,compare & update
  double totLen=0;
  THelixTrack myHlx(hlx);
  int iNode = -1;
  for (StvNodeIter it=trak->begin();it!=trak->end(); ++it) {
    iNode++;preNode=node; node = *it;
    StvNodePars sFP = node->GetFP(0);
    const StvHit *hit = node->GetHit();
    const float *hix = (hit)? hit->x():0;

    const double *X = node->mXDive;
    if (hit) {for (int i=0;i<3;i++) {dHit[i]=hix[i];};X = dHit;}
    double dS = myHlx.Path(X); myHlx.Move(dS,Fhlx);
    totLen+=dS;
    StvNodePars hFP; hFP.set(&myHlx,sFP._hz);
    hFP.convert(Fstv,Fhlx);
//??    StvFitErrs  sFE = node->GetFE(2);
    StvFitErrs  hFE; hFE.Set(&myHlx,sFP._hz);


    myXi2 = 6e6;
    if (hix) {//Hit is there. Calculate Xi2i etc...
      StvNodePars iFP(hFP); iFP._x=hix[0];iFP._y=hix[1];iFP._z=hix[2];
      StvFitPars  fp = hFP-iFP;

      const double *hRR = node->GetHE();
      myXi2 = fp.mH /(hFE.mHH+hRR[0]) *fp.mH;
      myXi2+= fp.mZ /(hFE.mZZ+hRR[2]) *fp.mZ;

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
   
//_____________________________________________________________________________
int StvKalmanTrackFitter::Clean(StvTrack *trak)
{
  int nErr = 0;

  for (StvNodeIter it=trak->begin();it!=trak->end();++it) 
  {
    int fail = 0;
    StvNode *node = *it;
    StvHit *hit = node->GetHit();
    if (!hit) continue;
    const StvNodePars &np = node->GetFP();
    if (fabs(np._ptin)    > mKons->mMaxPti) 	fail+= 1;
    if (fabs(np._curv)    > mKons->mMaxCurv) 	fail+= 2;
    if ( np.diff(hit->x())> mKons->mMaxRes) 	fail+= 4;
    if (!fail) continue;   
    node->SetXi2(3e33);node->SetHit(0); nErr++;
  }
  return nErr;
}

