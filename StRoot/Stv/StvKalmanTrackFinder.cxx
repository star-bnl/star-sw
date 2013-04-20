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
  double aveRes=0,aveXi2=0;
  mCurrTrak = 0;

  for (int isf=0;isf<(int)sfs->size();isf++) { //Loop over seed finders
    StvSeedFinder* sf = (*sfs)[isf];
    int myMinHits = kons->mMinHits;
//??    if(sf->Again()) myMinHits = kons->mGoodHits;
    for (int repeat =0;repeat<5;repeat++) {//Repeat search the same seed finder 
      nTrk = 0;nSeed=0; sf->Again();
      while ((mSeedHelx = sf->NextSeed())) 
      {
	nSeed++; nTally++;
						  if (sf->DoShow())  sf->Show();
	if (!mCurrTrak) mCurrTrak = kit->GetTrack();
	mCurrTrak->CutTail();	//Clean track from previous failure
	nAdded = FindTrack(0);
        sf->FeedBack(nAdded);
	if (!nAdded) 				continue;
	int ans = 0,fail=13;
//		Refit track   
        int nFitHits = mCurrTrak->GetNHits();
	do {
	  if (!mRefit) 				continue;
	  ans = Refit(1);
	  if (ans) 				break;
          nHits = mCurrTrak->GetNHits();
          if (nHits<=1) 			break;
	  nAdded = FindTrack(1);
          nHits = mCurrTrak->GetNHits();
          if (nHits<=3) 			break;
          if (nAdded<=0)			continue;;
// 			few hits added. Refit track to beam again 
	  ans = Refit(0);
	  if (ans) 				break;
	} while((fail=0));		
	nHits = mCurrTrak->GetNHits();
	if (nHits < myMinHits)	fail+=100;		;
        if (fail) nHits=0;
StvDebug::Count("FitRefit",nFitHits,nHits);
	if (fail) 	{//Track is failed, release hits & continue
	  mCurrTrak->CutTail();			continue;
        }

	StvNode *node = mCurrTrak->GetNode(StvTrack::kDcaPoint);
	if (node) node->UpdateDca();
static int akio=0;
if (akio && node) { 
 int ak = ThruFgt(node->GetFP());
 if (ak) {
   StvDebug::Break(-3);
 } }
 

	kit->GetTracks().push_back(mCurrTrak);
	nTrk++;nTrkTot++;
	aveRes += mCurrTrak->GetRes();
	aveXi2 += mCurrTrak->GetXi2();
	mCurrTrak=0;
      }
      nSeedTot+=nSeed;
      Info("FindTracks:","SeedFinder(%s) Seeds=%d Tracks=%d ratio=%d\%\n"
          ,sf->GetName(),nSeed,nTrk,(100*nTrk)/(nSeed+1));
      
      if (!nTrk && myMinHits == kons->mMinHits) break;
      myMinHits = kons->mMinHits;
    }//End of repeat
  }//End of seed finders

  if (nTrkTot) {aveRes/=nTrkTot; aveXi2/=nTrkTot;}
  Info("FindTracks","tracks=%d aveRes = %g aveXi2=%g",nTrkTot,aveRes,aveXi2);
  return nTrkTot;
}
//_____________________________________________________________________________
int StvKalmanTrackFinder::FindTrack(int idir)
{

static int nCall=0; nCall++;
static const StvConst *myConst = StvConst::Inst();
static       StvToolkit *kit      = StvToolkit::Inst();
static       StvFitter  *fitt     = StvFitter::Inst();

StvDebug::Break(nCall);
StvNodePars par[2];
StvFitErrs  err[2];
int mySkip=0,idive = 0,nNode=0,nHits=0;
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
  mDive->Set(par+0,err+0,idir);
  mDive->Set(par+1,err+1,&derivFit);	//Output of diving in par[1]


  while(idive==kDiveHits || idive==0) {

    do {//Stop tracking?
      idive = 99;
      if (!nNode)		continue;	//No nodes yet, OK
      mySkip = hitCount->Skip();
      if (!mySkip) 		continue;	//No Skip, OK
      if (idir)   		break;
      mySkip = hitCount->Reject();
      if (mySkip) 		break;
      mDive->SetSkip();
    } while ((idive=0));
    if (idive) 			break;

//+++++++++++++++++++++++++++++++++++++

    idive = mDive->Dive();

//+++++++++++++++++++++++++++++++++++++
    if (mySkip && !idive) {
      Warning("FindTrack","Strange case mySkip!=0 and iDive==0");
      break;
    }
    if (idive >= kDiveBreak) 			break;

    totLen+=mDive->GetLength();
    par[0]=par[1]; err[0]=err[1];			//pars again in par[0]
		// Stop tracking when too big Z or Rxy
    if (fabs(par[0]._z)  > myConst->mZMax  ) 	break;
    if (par[0].getRxy()  > myConst->mRxyMax) 	break;
    if (fabs(par[0]._curv)>myConst->mMaxCurv)	break;	
    if (fabs(par[0]._ptin)>myConst->mMaxPti)	break;	

    		
    const StvHits *localHits = 0; 
    if (idive== kDiveHits) {
static float gate[2]={myConst->mMaxWindow,myConst->mMaxWindow};   
      localHits = mHitter->GetHits(par,gate); 
    }


//	Create and add node to myTrak
    preNode = curNode;
    curNode = kit->GetNode();      
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
    innNode->SetELoss(eld,idir);
    err[0].Add(innNode->mELossData,par[0]);
    curNode->SetPre(par[0],err[0],0);
    innNode->SetDer(derivFit,idir);

    if (idive==kDiveDca) {
      curNode->SetType(StvNode::kDcaNode);
      double testDca = TCL::vdot(&par[0]._cosCA,par[0].P,2);
      assert(fabs(testDca)<1e-4);
      continue;
    }

    if (!localHits)	 continue;	//Never hits in curNode 
    curNode->SetHitPlane(mHitter->GetHitPlane());
    
    if (!localHits->size()) {//No hits in curNode
      hitCount->AddNit(); continue;
    } 
    fitt->Prep();
    double minXi2 = myConst->mXi2Hit,myXi2=3e33; 
    StvHit *minHit=0; int minIdx = -1;
    for (int ihit=0;ihit<(int)localHits->size();ihit++) {
      StvHit *hit = (*localHits)[ihit];
      myXi2 = fitt->Xi2(hit);
      if (myXi2 > minXi2) continue;
      minXi2 = myXi2; minHit = hit; minIdx = ihit;
    }
   
    if (minHit) {	// Fit succesful

      myXi2 = fitt->Xi2(minHit);
      int iuerr = fitt->Update(); 
      if (iuerr<=0 || (nHits<=3)) {		//Hit accepted
        hitCount->AddHit();nHits++;
        curNode->SetHE(fitt->GetHitErrs());
        curNode->SetFit(par[1],err[1],0);
        if (nHits>3) par[0]=par[1];
        err[0]=err[1]; 
      } else { minHit=0;}
    } 

    if (!minHit) {//No Hit or ignored
      myXi2 = 1e11;
      hitCount->AddNit(); 
    }
    curNode->SetHit(minHit); 
    curNode->SetXi2(myXi2,0);
    
  } // End Dive&Fitter loop 

  mCurrTrak->SetTypeEnd(mySkip);
  if (!idir) {
    double eff = hitCount->Eff(); if (eff){}
    int myReject = hitCount->Reject();
    if (myReject) {
      StvDebug::Count("hitCountRej2",myReject);
      mCurrTrak->ReleaseHits(); mCurrTrak->unset();
      kit->FreeTrack(mCurrTrak);mCurrTrak=0; return 0; }
  }
  if (nHits>3) {
    double tlen = mCurrTrak->GetLength();
    assert(tlen >0.0 && tlen<1000.);
  }


  return nHits;

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
      if (dca00 >1e11) StvDebug::Count("PrimNoDcaRej",    0);
      else             StvDebug::Count("PrimDca00Rej",dca00);
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
    StvDebug::Count("PrimXi2Acc",bestXi2);
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
static const double kEps = 1e-2;
enum {kTryFitMax = 5,kBadHits=5};

  int ans=0,anz=0,lane = 1;
  int& nHits = tkf->NHits();
  nHits = mCurrTrak->GetNHits();
  int nRepair = 3;
  if (nHits<= 5) {nRepair=0;} 
  if (nHits<=10) {nRepair=1;}
  int state = 0;
  StvNode *tstNode = (idir)? mCurrTrak->front(): mCurrTrak->back();
  for (int repair=0;repair<=nRepair;repair++)  	{ 	//Repair loop
    int converged = 0;
    for (int refIt=0; refIt<55; refIt++)  	{	//Fit iters
      ans = tkf->Refit(mCurrTrak,idir,lane,1);
//    ==================================
      nHits=tkf->NHits();
      if (nHits < kBadHits) break;
      if (ans>0) break;			//Very bad
      
      StvNodePars lstPars(tstNode->GetFP());	//Remeber params to compare after refit	
      anz = tkf->Refit(mCurrTrak,1-idir,1-lane,1); 
  //        ==========================================
      nHits=tkf->NHits();
      if (nHits < kBadHits) break;
      if (anz>0) break;	

      double dif = lstPars.diff(tstNode->GetFP());
      if ( dif < kEps) { converged = 1; break; } 
    }// End Fit iters
    
    state = (ans!=0) + 2*((anz!=0) + 2*(!converged) 
          + 2*(mCurrTrak->GetXi2()>kons->mXi2Trk)+2*(nHits <= kBadHits));
    if (!state) 		break;
    if (nHits <= kBadHits) 	break;
    StvNode *badNode=mCurrTrak->GetNode(StvTrack::kMaxXi2);
    if (!badNode) 		break;
    badNode->SetHit(0);
    nHits--; if (nHits < kBadHits) break;
  }//End Repair loop

  StvNode *node = mCurrTrak->GetNode(StvTrack::kDcaPoint);
  if (node) node->UpdateDca();
  if (ans<=0) state &= (-2);
  if (anz<=0) state &= (-4);
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
