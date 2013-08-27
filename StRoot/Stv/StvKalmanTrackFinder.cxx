#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"
#include "TCernLib.h"

#include "StvSeedFinder.h"
#include "StvKalmanTrackFinder.h"

#include "StvToolkit.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "StvUtil/StvDebug.h"
#include "StvTester.h"
#include "Stv/StvEnum.h"
#include "Stv/StvConst.h"
#include "Stv/StvDiver.h"
#include "Stv/StvHitter.h"
#include "Stv/StvFitter.h"
#include "Stv/StvTrackFitter.h"
#include "THelixTrack.h"
#include "Stv/StvDraw.h"
#include "Stv/StvStl.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
#include "Stv/StvHitCounter.h"
ClassImp(StvKalmanTrackFinder)

int ThruFgt(const StvNodePars &par);	///???????????????????????????????

typedef std::vector<StvNode*> 		StvNodeVec;
typedef std::map<double,StvNode*> 	StvNodeMap;
typedef StvNodeMap::iterator 		StvNodeMapIter ;

//_____________________________________________________________________________
StvKalmanTrackFinder::StvKalmanTrackFinder(const char *name):StvTrackFinder(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
  mDive = new StvDiver("KalmanTrackFinder");
  mDive->Init();
  double rMax,zMin,zMax;
  StTGeoProxy::Inst()->GetHitShape()->Get(zMin,zMax,rMax);
  if (zMax < -zMin) zMax = -zMin;
  mDive->SetRZmax(rMax,zMax);
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
static int nCall = 0; nCall++;
static int nTally = 0;
static StvToolkit *kit = StvToolkit::Inst();
static const StvConst  *kons = StvConst::Inst();
  int nTrk = 0,nTrkTot=0,nAdded=0,nHits=0,nSeed=0,nSeedTot=0;
  StvSeedFinders *sfs = kit->SeedFinders();
  double aveRes=0,aveXi2=0,aveHits=0;
  mCurrTrak = 0;

  for (int isf=0;isf<(int)sfs->size();isf++) { //Loop over seed finders
    mSeedFinder = (*sfs)[isf];
    int myMinHits = kons->mMinHits;
    for (int repeat =0;repeat<5;repeat++) {//Repeat search the same seed finder 
      nTrk = 0;nSeed=0; mSeedFinder->Again();
      while ((mSeedHelx = mSeedFinder->NextSeed())) 
      {
TVector3 dbSeed(mSeedHelx->Dir());
double seedEta = dbSeed.Eta(); 
StvDebug::Count("SeedEta",seedEta);

	nSeed++; nTally++;
	if (!mCurrTrak) mCurrTrak = kit->GetTrack();
	mCurrTrak->CutTail();	//Clean track from previous failure

//=============================
	nAdded = FindTrack(0);
//=============================

        mSeedFinder->FeedBack(nAdded);
	if (!nAdded) 				continue;
//		DebugDebugDebugDebugDebugDebugDebugDebug
StvDebug::Count("Xi2.geant",mCurrTrak->GetXi2());
StvDebug::Count("CALLS",1);
StvTrack geanTrak(*mCurrTrak);
//		DebugDebugDebugDebugDebugDebugDebugDebug

StvDebug::Count("SeedDive",seedEta);

	int ans = 0,fail=13;
//		Refit track   
        int nFitHits = mCurrTrak->GetNHits();
	do {
	  if (!mRefit) 				continue;
          if(nFitHits<3)			break;
//=============================
	  ans = Refit(1);
//=============================
	  if (ans) 				break;
StvDebug::Count("CALLS",2);
StvDebug::Count("Xi2.refit",mCurrTrak->GetXi2());
StvTrack refiTrak(*mCurrTrak);
          nHits = mCurrTrak->GetNHits();
          if (nHits<=3) 			break;
//=============================
	  nAdded = FindTrack(1);
//=============================
          if (nAdded<=0)			continue;;
StvDebug::Count("CALLS",3);
          nHits = mCurrTrak->GetNHits();
          if (nHits<=3) 			break;
StvDebug::Count("Xi2.backw",mCurrTrak->GetXi2());
StvTrack bakwTrak(*mCurrTrak);
// 			few hits added. Refit track to beam again 
//=============================
	  ans = Refit(0);
//=============================
	  if (ans) {
	    *mCurrTrak = refiTrak; 
	    nHits = mCurrTrak->SetUsed();
	  }
else {StvDebug::Count("CALLS",4);}

StvDebug::Count("Xi2.REFIT",mCurrTrak->GetXi2());
	} while((fail=0));		
	nHits = mCurrTrak->GetNHits();
	if (nHits < myMinHits)	fail+=100;		;
if (!fail) StvDebug::Count("NumCalls",5);
        if (fail) nHits=0;
	if (fail) 	{//Track is failed, release hits & continue
	  mCurrTrak->CutTail();			continue;
        }
StvDebug::Count("CALLS",5);
	StvNode *node = MakeDcaNode(mCurrTrak); if(node){};
 

	kit->GetTracks().push_back(mCurrTrak);
	nTrk++;nTrkTot++;
        aveHits+= nHits;
	aveRes += mCurrTrak->GetRes();
	aveXi2 += mCurrTrak->GetXi2();
	mCurrTrak=0;
      }
      nSeedTot+=nSeed;
      Info("FindTracks:","SeedFinder(%s) Seeds=%d Tracks=%d ratio=%d\%\n"
          ,mSeedFinder->GetName(),nSeed,nTrk,(100*nTrk)/(nSeed+1));
      
      if (!nTrk && myMinHits == kons->mMinHits) break;
      myMinHits = kons->mMinHits;
    }//End of repeat
  }//End of seed finders

  if (nTrkTot) {aveHits/=nTrkTot;aveRes/=nTrkTot; aveXi2/=nTrkTot;}
  Info("FindTracks","tracks=%d aveHits = %g aveRes = %g aveXi2=%g",nTrkTot,aveHits,aveRes,aveXi2);
  return nTrkTot;
}
//_____________________________________________________________________________
int StvKalmanTrackFinder::FindTrack(int idir)
{

static int nCall=0; nCall++;
static int nTally=0; 
static const StvConst *myConst = StvConst::Inst();
static       StvToolkit *kit      = StvToolkit::Inst();
static       StvFitter  *fitt     = StvFitter::Inst();

StvDebug::Break(nCall);
StvNodePars par[2];
StvFitErrs  err[2];
int mySkip=0,idive = 0,nNode=0,nHits=0,nTotHits=0;
double totLen=0;
StvNode *curNode=0,*preNode=0,*innNode=0,*outNode=0;
StvHitCounter *hitCount = StvHitCounter::Inst();
hitCount->Clear();
  
  if (mCurrTrak->empty()) {//Track empty, Backward tracking, to beam
    assert(!idir);
    double Hz = kit->GetHz(mSeedHelx->Pos());
    par[0].set(mSeedHelx,Hz); 		//Set seed pars into par[0] and err[0]
    err[0].Set(mSeedHelx,Hz); err[0]*= kKalmanErrFact; 
    par[0].reverse();			//Seed direction OutIn but track direction is allways InOut	
    err[0].Backward();
  } else 	{//Forward or backward tracking
 
    curNode =(idir)? mCurrTrak->back(): mCurrTrak->front();
    par[0] = curNode->GetFP(); err[0] = curNode->GetFE(); 	//Set outer node pars into par[0]
    nTotHits = mCurrTrak->GetNFits(idir);
  }

//  	Skip too big curvature or pti
  if (fabs(par[0]._curv)>myConst->mMaxCurv)	return 0;	
  if (fabs(par[0]._ptin)>myConst->mMaxPti)	return 0;	

//  	skip P too small
  { double t = par[0]._tanl, pti = par[0]._ptin;	
    if ((t*t+1.)< myConst->mMinP2*pti*pti)	return 0;
  }
  fitt->Set(par, err, par+1,err+1);
  mHitter->Reset();
StvFitDers derivFit;
  mDive->Reset();
  mDive->SetOpt(StvDiver::kTargHit | StvDiver::kDoErrs | StvDiver::kTarg2D);
  mDive->Set(par+0,err+0,idir);
  mDive->Set(par+1,err+1,&derivFit);	//Output of diving in par[1]


  while((idive & StvDiver::kDiveHits) || idive==0) {

    do {//Stop tracking?
      idive = 99;
      if (!nNode)		continue;	//No nodes yet, OK
      mySkip = hitCount->Skip();
      if (mySkip) 		break;		//Skip stop tracking,
    } while ((idive=0));
    if (idive) 			break;

//+++++++++++++++++++++++++++++++++++++
    nTally++;
    idive = mDive->Dive();
//+++++++++++++++++++++++++++++++++++++
    if (idive & StvDiver::kDiveBreak) 		break;
    if (idive & StvDiver::kDiveDca  ) 		break;

    totLen+=mDive->GetLength();
    par[0]=par[1]; err[0]=err[1];			//pars again in par[0]
		// Stop tracking when too big Z or Rxy
    if (fabs(par[0]._z)  > myConst->mZMax  ) 	break;
    if (par[0].getRxy()  > myConst->mRxyMax) 	break;
    if (fabs(par[0]._curv)>myConst->mMaxCurv)	break;	
    if (fabs(par[0]._ptin)>myConst->mMaxPti)	break;	

    		
    const StvHits *localHits = 0; 
    if (idive & StvDiver::kDiveHits) {
static float gate[4]={myConst->mCoeWindow,myConst->mCoeWindow
                     ,myConst->mMaxWindow,myConst->mMaxWindow};   
      localHits = mHitter->GetHits(par,err,gate); 
    }


//	Create and add node to myTrak
    preNode = curNode;
    curNode = kit->GetNode();      
    assert(preNode != curNode);
    assert(curNode != mCurrTrak->front());
    assert(curNode != mCurrTrak->back ());

    if (!idir)  {mCurrTrak->push_front(curNode);innNode=curNode;outNode=preNode;}
    else        {mCurrTrak->push_back (curNode);innNode=preNode;outNode=curNode;}
    nNode++;		// assert(nNode<200);
    if (nNode>200) { //Something very wrong
      Error("FindTrack","Too many nodes =200 Skip track");
      return 0;
    }

    curNode->mLen = (!idir)? totLen:-totLen;
		// Set prediction
    StvELossData eld = mDive->GetELossData();
    eld.mTally = nTally;
    innNode->SetELoss(eld,idir);
    err[0].Add(innNode->mELossData,par[0]);
    err[0].Recov();
    curNode->SetXDive(par[0]);
    curNode->SetPre(par[0],err[0],0);
    innNode->SetDer(derivFit,idir);

    if (!localHits)	 continue;	//Never hits in curNode 
    curNode->SetHitPlane(mHitter->GetHitPlane());
    
    if (!localHits->size()) {//No hits in curNode
      hitCount->AddNit(); continue;
    } 
    fitt->Prep();
    double  minXi2[2]={1e11,1e11},myXi2;
    StvHit *minHit[2]={0};
    minXi2[0] = myConst->mXi2Hit,myXi2=3e33; 
    int minIdx = -1;
    for (int ihit=0;ihit<(int)localHits->size();ihit++) {
      StvHit *hit = (*localHits)[ihit];
      myXi2 = fitt->Xi2(hit);
      if (nTotHits > 5 && fitt->IsFailed() == -99) { // Too big track errs
         mySkip = 4; break;	//Track Errors too big, stop tracking
      }
      if (myXi2 > minXi2[0]) continue;
      minXi2[1]=minXi2[0]; minXi2[0] = myXi2;
      minHit[1]=minHit[0]; minHit[0] = hit; minIdx = ihit;
    }
    if (mySkip) break; 		//Track Errors too big
    curNode->SetMem(minHit ,minXi2);
    if (minHit[0]) {	// Fit succesful
      
      myXi2 = fitt->Xi2(minHit[0]);
      int iuerr = fitt->Update(); 
      if (iuerr<=0 || (nHits<=3)) {		//Hit accepted
        hitCount->AddHit();
	nHits++;nTotHits++;assert(nHits<=100);
        curNode->SetHE(fitt->GetHitErrs());
        curNode->SetFit(par[1],err[1],0);
        par[0]=par[1];
        err[0]=err[1]; 
      } else { minHit[0]=0;}
    } else 		{//No Hit or ignored
      myXi2 = 1e11;
      hitCount->AddNit(); 
    }
    curNode->SetHit(minHit[0]); 
    curNode->SetXi2(myXi2,0);
    
  } // End Dive&Fitter loop 

  mCurrTrak->SetTypeEnd(mySkip);
  if (!idir) {
    double eff = hitCount->Eff(); if (eff){}
    int myReject = hitCount->Reject();
StvDebug::Count("hitCountReject",myReject);
   if (myReject) {
// StvDebug::Count("RejectXi2cm2",mSeedFinder->GetXi2(0));
// StvDebug::Count("RejectXi2   ",mSeedFinder->GetXi2(1));

     mCurrTrak->CutTail(); return 0; }
  }
// StvDebug::Count("AcceptXi2cm2",mSeedFinder->GetXi2(0));
// StvDebug::Count("AcceptXi2   ",mSeedFinder->GetXi2(1));
  if (nHits>3) {
    double tlen = mCurrTrak->GetLength();
    assert(tlen >0.0 && tlen<1000.);
  }


  return nHits;

}
//_____________________________________________________________________________
int StvKalmanTrackFinder::Swim(int idir, int opt, const double target[3]
                               ,const StvNodePars *inpPar,const StvFitErrs *inpErr
                               ,      StvNodePars *outPar,      StvFitErrs *outErr
			       ,       StvFitDers *derivFit)
{

static int nCall=0; nCall++;

  mDive->Reset();
  mDive->Set(inpPar,inpErr,idir);
  mDive->Set(outPar,outErr,derivFit);	//Output of diving in par[1]
  mDive->SetOpt(opt);
  int nTg = (opt & StvDiver::kTarg3D)? 3:2; 
  if (target) mDive->SetTarget(target,nTg);


//+++++++++++++++++++++++++++++++++++++

  int idive = mDive->Dive();

//+++++++++++++++++++++++++++++++++++++
  return idive;

}
//_____________________________________________________________________________
StvNode *StvKalmanTrackFinder::MakeDcaNode(StvTrack *tk)
{
static StvToolkit *kit = StvToolkit::Inst();

  StvNode *start = tk->front();
  int opt = StvDiver::kTarg2D | StvDiver::kDoErrs;
  StvNodePars dcaPars;
  StvFitErrs  dcaErrs;
  StvFitDers  dcaDers;
  int iSwim = Swim(0,opt,0
                  ,&(start->GetFP()),&(start->GetFE())
		  ,&dcaPars,&dcaErrs,&dcaDers);
  if (!(iSwim & StvDiver::kDiveDca)) return 0;		   

  StvNode *dcaNode = kit->GetNode();      
  tk->push_front(dcaNode);
  dcaNode->mLen =  start->mLen + mDive->GetLength();
	       // Set prediction
  StvELossData eld = mDive->GetELossData();
  dcaNode->SetELoss(eld,0);
  dcaErrs.Add(dcaNode->mELossData,dcaPars);
  dcaNode->SetPre(dcaPars,dcaErrs,0);
  dcaNode->SetDer(dcaDers,0);
  dcaNode->SetXi2(1e11,0);
  dcaNode->SetType(StvNode::kDcaNode);
  double testDca = TCL::vdot(&dcaPars._cosCA,dcaPars.P,2);
  assert(fabs(testDca)<1e-4);

  return dcaNode;
}
//_____________________________________________________________________________
int StvKalmanTrackFinder::FindPrimaries(const StvHits &vtxs)	
{
static const StvConst *myConst =   StvConst::Inst();
static     StvToolkit *kit     = StvToolkit::Inst();
static     StvTrackFitter *tkf = StvTrackFitter::Inst();

  StvTracks &traks = kit->GetTracks();
  int goodCount= 0, plus=0, minus=0;
  int nVertex = vtxs.size();  
  if (!nVertex) return 0;
  int nTracks = 0;
  for (StvTrackIter it=traks.begin(); it!=traks.end() ;++it) {
    StvTrack *track = *it;  nTracks++;
    double dca00 = track->ToBeam();
    if (dca00 > myConst->mDca2dZeroXY) {
      continue;
    }
    int bestVertex=-1; double bestXi2 = myConst->mXi2Vtx;
    for (int iVertex=0;iVertex<nVertex;iVertex++) {
      StvHit *vertex = vtxs[iVertex];
      if (tkf->Fit(track,vertex,0)) 		continue;
      double Xi2 = tkf->GetXi2();
      if (Xi2>=bestXi2) 			continue;
// 		Found better Xi2
      bestXi2 = Xi2; bestVertex=iVertex;
    }//End vertex loop
    
    if(bestVertex<0) 				continue;
    StvNode *node = kit->GetNode();
    StvHit *hit = vtxs[bestVertex];
    hit->addCount();
    tkf->Fit(track,hit,node);
    track->push_front(node);
    track->SetPrimary(bestVertex+1);
    node->SetType(StvNode::kPrimNode);    
    node->SetHit(hit);    
    node->SetXi2(bestXi2,0);
    goodCount++;
    if (track->GetCharge()>0) { plus++; } else { minus++; }

  }//End track loop 
  return goodCount;
}
//_____________________________________________________________________________
int StvKalmanTrackFinder::Refit(int idir)
{
static int nCall=0;nCall++;
static StvTrackFitter *tkf = StvTrackFitter::Inst();
static const StvConst  *kons = StvConst::Inst();
static const double kEps = 1.e-2,kEPS=1e-1;

  int ans=0,anz=0,lane = 1;
  int& nHits = tkf->NHits();
  nHits = mCurrTrak->GetNHits();
  int nBegHits = nHits;
  int nRepair =(nHits-5)*0.1;
  int state = 0;
  StvNode *tstNode = (idir)? mCurrTrak->front(): mCurrTrak->back();
  int nIters = 0,nDrops=0;
  for (int repair=0;repair<=nRepair;repair++)  	{ 	//Repair loop
    int converged = 0;
    for (int refIt=0; refIt<55; refIt++)  	{	//Fit iters
      nIters++;
      ans = tkf->Refit(mCurrTrak,idir,lane,1);
StvDebug::Count("CALLS",10);
//    ==================================
      nHits=tkf->NHits();
      if (nHits < kons->mMinHits) break;
      if (ans>0) break;			//Very bad
      
      StvNodePars lstPars(tstNode->GetFP());	//Remeber params to compare after refit	
      anz = tkf->Refit(mCurrTrak,1-idir,1-lane,1); 
StvDebug::Count("CALLS",10);
  //        ==========================================
      nHits=tkf->NHits();
      if (nHits < kons->mMinHits) break;
      if (anz>0) break;	

      double dif = lstPars.diff(tstNode->GetFP(),tstNode->GetFE());
      double eps = (ans || anz)? kEPS:kEps;
      if ( dif < eps) { //Fit converged
      converged = 1; break; } 

    }// End Fit iters
    
    state = (ans!=0) + 10*((anz!=0) + 10*((!converged) 
          + 10*((mCurrTrak->GetXi2()>kons->mXi2Trk)+10*(nHits < kons->mMinHits))));
    if (!state) 		break;
    if (nHits < kons->mMinHits) 	break;
    StvNode *badNode=mCurrTrak->GetNode(StvTrack::kMaxXi2);
    if (!badNode) 		break;
    badNode->SetHit(0); nDrops++;
    nHits--; if (nHits < kons->mMinHits) { state = 1000000; break;}
  }//End Repair loop

if (state) {
StvDebug::Count("RejectXi2cm2",mSeedFinder->GetXi2(0));
StvDebug::Count("RejectXi2   ",mSeedFinder->GetXi2(1));
} else {
StvDebug::Count("AcceptXi2cm2",mSeedFinder->GetXi2(0));
StvDebug::Count("AcceptXi2   ",mSeedFinder->GetXi2(1));
}

  if (ans<=0) state &= (-2);
  if (anz<=0) state &= (-4);
  nDrops = nBegHits-nHits;
  if (!state) {
    if (nDrops==0) StvDebug::Count("RefitGoodIters",nIters);      
    else           StvDebug::Count("RefitBaddIters",nIters); 
    StvDebug::Count("RefitDrops",100.*nDrops/nBegHits); 
  } else           StvDebug::Count("RefitFailed"   ,nIters);    
  return state;

}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
// int checkfgt(double x0, double y0, double x1, double y1){
//   static const double z=67.399; //disc1 z
//   double x=x0+x1*z;
//   double y=y0+y1*z;
//   double r=sqrt(x*x+y*y);
//   if(r>12.0 && r<38.0) return 1;
//   return 0;
// }

int ThruFgt(const StvNodePars &par)
{
static const double kZ=67.399; //disc1 z
static const double kRmin =12.0;
static const double kRmax =38.0;
  double t = (kZ-par._z)/par._tanl;
  if (t<0) return 0;
  double myX = par._x + par._cosCA*t;  
  double myY = par._y + par._sinCA*t;
  double rr = myX*myX+myY*myY;
  if (rr<kRmin*kRmin) return 0;
  if (rr>kRmax*kRmax) return 0;
  return 1;
}  
