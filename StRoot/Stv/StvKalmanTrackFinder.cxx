#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"
#include "TSystem.h"
#include "TCernLib.h"
#include "TGeoManager.h"

#include "StvSeedFinder.h"
#include "StvKalmanTrackFinder.h"

#include "StvToolkit.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "StvUtil/StvELossTrak.h"
#include "StvUtil/StvDebug.h"
#include "StvTester.h"
#include "Stv/StvEnum.h"
#include "Stv/StvConst.h"
#include "Stv/StvDiver.h"
#include "Stv/StvHitter.h"
#include "Stv/StvFitter.h"
#include "Stv/StvTrackFitter.h"
#include "Stv/StvStl.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
#include "Stv/StvHitCounter.h"
ClassImp(StvKalmanTrackFinder)
static int dbMask=0;

typedef std::vector<StvNode*> 		StvNodeVec;
typedef std::map<double,StvNode*> 	StvNodeMap;
typedef StvNodeMap::iterator 		StvNodeMapIter ;
//static const double k57 = TMath::RadToDeg();
static int BOTOHO = (gSystem->Getenv("BOTOHO"))? atoi(gSystem->Getenv("BOTOHO")):0;
//_____________________________________________________________________________
StvKalmanTrackFinder::StvKalmanTrackFinder(const char *name):StvTrackFinder(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
  mDive = StvDiver::Inst();
  mHitter = new StvHitter();
  mHitCounter = new StvHitCounter();
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
void StvKalmanTrackFinder::SetCons(const StvKonst_st* k)
{
  mKons = k;
  double rMax,zMin,zMax;
  StTGeoProxy::Inst()->GetHitShape()->Get(zMin,zMax,rMax);
  if (zMax < -zMin) zMax = -zMin;
//???????????????????????????????????????????????????????
  rMax = 210; zMax = 220;
//???????????????????????????????????????????????????????

  mDive->SetRZmax(rMax,zMax);
  mHitCounter->SetCons(mKons);
  mDive->SetRZmax(mKons->mRxyMax,mKons->mZMax);
}
//_____________________________________________________________________________
int StvKalmanTrackFinder::FindTracks()
{
static int nCall = 0; nCall++;
static int nTally = 0;
static StvToolkit *kit = StvToolkit::Inst();
enum {kRepeatSeedFinder = 2};

  int nTrk = 0,nTrkTot=0,nAdded=0,nHits=0,nSeed=0,nSeedTot=0;
  StvSeedFinders *seedFinders = kit->SeedFinders();
  double aveRes=0,aveXi2=0,aveHits=0;
  mCurrTrak = 0;

  for (int seedFinder=0;seedFinder<(int)seedFinders->size();seedFinder++) { //Loop over seed finders
    mSeedFinder = (*seedFinders)[seedFinder];
    for (int repeat =0;repeat<kRepeatSeedFinder;repeat++) {//Repeat search the same seed finder 
      nTrk = 0;nSeed=0; mSeedFinder->Again(repeat);
      int ans = 0,fail=13;
      while ((mSeedHelx = mSeedFinder->NextSeed())) 
      {

        if (BOTOHO&64) mSeedFinder->DrawHelix();
	nSeed++; nTally++; 

	if (!mCurrTrak) mCurrTrak = kit->GetTrack();
	mCurrTrak->CutTail();	//Clean track from previous failure
        mSeedFinder->Init(mCurrTrak);
        fail = 13;
        do {
	nHits = FindTrack(0);
        
//=============================
        fail = 1;
        if (nHits<3) break;
        StvDebug::Count("FindTrack_0",nAdded);
   	if (BOTOHO&1) StvDebug::Zhow(mCurrTrak);
//=============================
//		Refit track   
        if (nHits>3) {
          ans = Refit(1);
          if (BOTOHO&2) StvDebug::Zhow(mCurrTrak);
        } 
//=============================
	nAdded = FindTrack(1);
        if (nAdded) {
          StvDebug::Count("FindTrack_1",nAdded);
          if (BOTOHO&4) StvDebug::Zhow(mCurrTrak);
	  ans = Refit(2);
	  fail = 4;
	  if (ans) break;
          StvDebug::Count("Refit_1",mCurrTrak->GetNHits());
          if (BOTOHO&8) StvDebug::Zhow(mCurrTrak);
        }
        nHits = mCurrTrak->GetNHits();
        fail = 6; if (nHits<mKons->mMinHits) 	break;
        if (BOTOHO&16) StvDebug::Zhow(mCurrTrak);


      } while((fail=0));		
      
      if (fail) 	{//Track is failed, release hits & continue
          mSeedFinder->FeedBack(0);
	  mCurrTrak->CutTail();			
	  continue;
      }
      StvNode *node = MakeDcaNode(mCurrTrak); if(node){};

      mSeedFinder->FeedBack(mCurrTrak);
if (node) {
  double pt = node->GetFP().getPt();
  if (pt<3) StvDebug::Count("Pt",pt);
}
      mCurrTrak->AddId(10*seedFinder+repeat);
      mCurrTrak->SetUsed();
      kit->GetTracks().push_back(mCurrTrak);
      nTrk++;nTrkTot++;
      aveHits+= nHits;
      aveRes += mCurrTrak->GetRes();
      aveXi2 += mCurrTrak->GetXi2();
      mCurrTrak=0;
    }
    nSeedTot+=nSeed;
    Info("FindTracks:","SeedFinder(%s) Seeds=%d Tracks=%d ratio=%d\n"
        ,mSeedFinder->GetName(),nSeed,nTrk,(100*nTrk)/(nSeed+1));
      
    if (!nTrk ) break;
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
static StvToolkit *kit = StvToolkit::Inst();
static StvFitter  *fitt= StvFitter::Inst();
const StvHit *prevHit = 0;
StvNodePars par[2];
StvFitErrs  err[2];
int skip=0,idive = 0,nNode=0,nHits=0,nTotHits=0;
double totLen=0;
StvNode *curNode=0,*preNode=0,*innNode=0,*outNode=0;
const StHitPlane *prevHitPlane=0;

  mDive->Reset();
  
  if (idir==0) {//Track with seed, Backward tracking, to beam
    mHitCounter->Clear();
    CountHits(mCurrTrak);
    curNode = mCurrTrak->front();
    par[0] = curNode->GetFP(0);
    err[0] = curNode->GetFE(0);
    nHits = mCurrTrak->GetNHits();
    mHitIdTruth = 0;
    if (mIdTruth) { mHitIdTruth = curNode->GetHit()->idTru();}
  } else 	{//Forward or backward tracking
 
    curNode = mCurrTrak->back();
    par[0] = curNode->GetFP(); err[0] = curNode->GetFE(); 	//Set outer node pars into par[0]
    nTotHits = mCurrTrak->GetNHits();
  }
  prevHitPlane = curNode->GetHitPlane();
  if (prevHitPlane) {
    mDive->SetPrev(prevHitPlane->GetName());	//define previous volume name to avoid doubling
    prevHit = curNode->GetHit();
  }

//  	Skip too big curvature or pti
  if (fabs(par[0].getCurv())>mKons->mMaxCurv)	return 0;	
  if (fabs(par[0].getPtin())>mKons->mMaxPti )	return 0;	
//  	skip P too small
  if (par[0].getP2() <  mKons->mMinP2) 		return 0;
  
  fitt->Set(par, err, par+1,err+1);
  mHitter->Reset();
  StvFitDers derivFit;
//		We need here to find a hit and calulate errs. No Dca yet
  mDive->SetOpt(StvDiver::kTargHit | StvDiver::kDoErrs);
  mDive->Set(par+0,err+0,idir);
  mDive->Set(par+1,err+1,&derivFit);	//Output of diving in par[1]

  do {// propagating along the track

//+++++++++++++++++++++++++++++++++++++
    nTally++;
    idive = mDive->Dive();
//+++++++++++++++++++++++++++++++++++++
    skip = 0;
    double deltaL = mDive->GetLength();
    totLen+=deltaL;
    par[0]=par[1]; err[0]=err[1];			//pars again in par[0]
    if (idive & StvDiver::kDiveBreak) 		{skip = 11;	break;}
    if (idive & StvDiver::kDiveDca  ) 		{skip = 12;	break;}
		// Stop tracking when too big Z or Rxy
    if (fabs(par[0].getZ()) > mKons->mZMax  ) 	{skip = 13;	break;}
    if (par[0].getRxy()     > mKons->mRxyMax) 	{skip = 14;	break;}
    if (fabs(par[0].getCurv())>mKons->mMaxCurv)	{skip = 15;	break;}	
    if (fabs(par[0].getPtin())>mKons->mMaxPti)	{skip = 16;	break;}	
        		
    const StvHits *localHits = 0; 
    if (idive & StvDiver::kDiveHits) {
      float gate[4]={mKons->mCoeWindow,mKons->mCoeWindow
                     ,mKons->mMaxWindow,mKons->mMaxWindow};   

      localHits = mHitter->GetHits(par,err,gate); 
      if (localHits && prevHitPlane == mHitter->GetHitPlane()) continue;
    }


//	Create and add node to myTrak
    preNode = curNode;
    curNode = kit->GetNode();      

    if (!idir)  {mCurrTrak->push_front(curNode);innNode=curNode;outNode=preNode;}
    else        {mCurrTrak->push_back(curNode) ;innNode=preNode;outNode=curNode;}
    if (outNode){}
    nNode++;		
    if (nNode>2560) { //Something very wrong
      Error("FindTrack","Too many nodes =2560 Skip track");
      skip = 17;	break; ///???
      assert (0 && "Too many nodes =2560");
      return 0;
    }

    curNode->mLen = (!idir)? totLen:-totLen;
		// Set prediction

    StvELossTrak *eld = mDive->TakeELoss();
    if (eld->Len()>0) {
      innNode->SetELoss(eld,idir);
    } else {
      kit->FreeELossTrak(eld); eld = 0;
    }
    curNode->SetXDive(par[0]);
    curNode->SetPre(par[0],err[0],0);
    innNode->SetDer(derivFit,idir);

    curNode->SetHitPlane(mHitter->GetHitPlane());
    
    if (!localHits)	 continue;	//Never hits in curNode 
    if (!localHits->size()) {//No hits in curNode
      skip = mHitCounter->AddNit();
      if (skip) break;
      continue;
    } 
    prevHitPlane = mHitter->GetHitPlane();

    fitt->Prep();
    double  minXi2[2]={1e11,1e11},myXi2;
    StvHit *minHit[2]={0};
    minXi2[0] = mKons->mXi2Hit,myXi2=3e33; 
    int minIdx = -1;
    for (int ihit=0;ihit<(int)localHits->size();ihit++) {
      StvHit *hit = (*localHits)[ihit];
      if (mHitIdTruth && mHitIdTruth != hit->idTru()) continue;
      if (hit == prevHit) continue;
      myXi2 = fitt->Xi2(hit);


      if (nTotHits > mKons->mMinHits && fitt->IsFailed() == -99) { // Too big track errs
         skip = 17; break;	//Track Errors too big, stop tracking
      }
      if (myXi2 > minXi2[1]) continue;
      if (myXi2 < minXi2[0]) {
        minXi2[1]=minXi2[0]; minXi2[0] = myXi2;
        minHit[1]=minHit[0]; minHit[0] = hit; minIdx = ihit;}
      else {
        minXi2[1] = myXi2; minHit[1]= hit;
      }
    }
    if (minIdx){};

    curNode->SetMem(minHit ,minXi2);
    if (minHit[0] ) 		{	// Fit succesful
      assert(minHit[0] != prevHit); 
      prevHit  = minHit[0]; 
      myXi2 = fitt->Xi2(minHit[0]);
      int iuerr = fitt->Update(); 
      if (iuerr<=0 || (nHits<3)) {		//Hit accepted

if (dbMask&1) {
        TVector3 vhit(minHit[0]->x());
        TVector3 vdir(par[0]._d); 
        TVector3 vpos0(par[0]._x); 
        TVector3 vpos1(par[1]._x); 
        double dis0 = (vpos0-vhit).Mag2()- pow((vpos0-vhit).Dot(vdir),2);
        dis0 = sqrt(dis0);
        double dis1 = (vpos1-vhit).Mag2()- pow((vpos1-vhit).Dot(vdir),2);
        dis1 = sqrt(dis1);
        printf("OLD=%g NOW=%g dif=%g\n",dis0,dis1,dis0-dis1);
}

        mHitCounter->AddHit();
	nHits++;nTotHits++;assert(nHits<2560);
        curNode->SetHE(fitt->GetHitErrs());
        curNode->SetFit(par[1],err[1],0);
double along = TVector3(par[0].dir()).Dot(TVector3(par[0].dir()));
assert(along>0.5);
        par[0]=par[1];
        err[0]=err[1]; 
static int myDebug = 0;
        if(myDebug) par[0].print("Fitted");
      } else { minHit[0]=0;}
    } else 		{//No Hit or ignored
      myXi2 = 1e11;
      skip = mHitCounter->AddNit(); 
      if (skip) break;
    }
    curNode->SetHit(minHit[0]); 
    curNode->SetXi2(myXi2,0);
assert(vsuma(curNode->GetFE(0).TkDir()[0],3*3)>0.1);

  } while((idive & StvDiver::kDiveHits) || idive==0); // End Dive&Fitter loop 

  mCurrTrak->SetTypeEnd(skip);
  if (!idir) {
    double eff = mHitCounter->Eff(); if (eff){}
    int myReject = mHitCounter->Reject();
if(myReject) StvDebug::Count("EndTrk",myReject);
    if (myReject) { mCurrTrak->CutTail(); return 0; }
  }
if (skip && !idir) StvDebug::Count("EndTrk",skip);


  mHitCounter->Reset();
  return nHits;

}
//_____________________________________________________________________________
void StvKalmanTrackFinder::CountHits(const StvTrack *tk)
{  
  for (StvNodeConstIter it=tk->begin();it!=tk->end(); ++it) {
    StvNode *node = *it;
    const StvHit *hit= node->GetHit();
    if (hit) {mHitCounter->AddHit();} else {mHitCounter->AddNit();} 
  }
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
//		We search DCA point + errors only (no hits)
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
  double diveLen=mDive->GetLength();
  dcaNode->mLen =  start->mLen + diveLen;
	       // Set prediction
  StvELossTrak *eld = mDive->TakeELoss();
  if (eld->Len()>0) {
    dcaNode->SetELoss(eld,0);
    dcaPars.add(eld,-diveLen);
    dcaErrs.Add(eld,-diveLen);
  } else {
    kit->FreeELossTrak(eld);
  }
  dcaNode->SetPre(dcaPars,dcaErrs,0);
  dcaNode->SetDer(dcaDers,0);
  dcaNode->SetXi2(1e11,0);
  dcaNode->SetType(StvNode::kDcaNode);
  double testDca = TCL::vdot(dcaPars.dir(),dcaPars.pos(),2);
  assert(fabs(testDca)<1e-3*(1+fabs(dcaNode->mLen)));

  return dcaNode;
}
//_____________________________________________________________________________
int StvKalmanTrackFinder::FindPrimaries(const StvHits &vtxs)	
{
static     StvToolkit *kit     = StvToolkit::Inst();

  StvTracks &traks = kit->GetTracks();
  int goodCount= 0, plus=0, minus=0;
  int nVertex = vtxs.size();  
  if (!nVertex) return 0;
  int nTracks = 0;
  for (StvTrackIter it=traks.begin(); it!=traks.end() ;++it) {
    StvTrack *track = *it;  nTracks++;
    double dca00 = track->ToBeam();
    if (dca00 > mKons->mDca2dZeroXY) {
      continue;
    }
    int bestVertex=-1; double bestXi2 = mKons->mXi2Vtx;
    for (int iVertex=0;iVertex<nVertex;iVertex++) {
      StvHit *vertex = vtxs[iVertex];
      if (mTrackFitter->Fit(track,vertex,0)) 		continue;
      double Xi2 = mTrackFitter->GetXi2();
      if (Xi2>=bestXi2) 			continue;
// 		Found better Xi2
      bestXi2 = Xi2; bestVertex=iVertex;
    }//End vertex loop
    
    if(bestVertex<0) 				continue;
    StvNode *node = kit->GetNode();
    StvHit *hit = vtxs[bestVertex];
    hit->addCount();
    mTrackFitter->Fit(track,hit,node);
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
int StvKalmanTrackFinder::Refit(int mode)
{
  int dir,lane,numb;
  switch ( mode ) {
    case 0: dir = 1; lane = 0; numb = 2; break; 
    case 1: dir = 1; lane = 1; numb = 1; break;
    case 2: dir = 1; lane = 0; numb = 2; break;
    default: assert(0);
  }

  int ians = mTrackFitter->RefitLoop(mCurrTrak,dir,lane,numb);
  if (BOTOHO&32) StvDebug::Zhow(mCurrTrak);
  return ians;
}
