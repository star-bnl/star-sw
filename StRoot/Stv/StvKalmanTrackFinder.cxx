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
#include "Stv/StvDraw.h"
#include "Stv/StvStl.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
#include "Stv/StvHitCounter.h"
ClassImp(StvKalmanTrackFinder)


typedef std::vector<StvNode*> 		StvNodeVec;
typedef std::map<double,StvNode*> 	StvNodeMap;
typedef StvNodeMap::iterator 		StvNodeMapIter ;
static const double k57 = TMath::RadToDeg();
static const char *BOTOHO = gSystem->Getenv("BOTOHO");
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

double ptBefRefit=0,ptAftRefit=0;
int    nHitsBefRefit=0,nHitsAftRefit=0;



  int nTrk = 0,nTrkTot=0,nAdded=0,nHits=0,nSits,nSeed=0,nSeedTot=0,myMask=0;
  StvSeedFinders *seedFinders = kit->SeedFinders();
  double aveRes=0,aveXi2=0,aveHits=0;
  mCurrTrak = 0;

  for (int seedFinder=0;seedFinder<(int)seedFinders->size();seedFinder++) { //Loop over seed finders
    mSeedFinder = (*seedFinders)[seedFinder];
    for (int repeat =0;repeat<kRepeatSeedFinder;repeat++) {//Repeat search the same seed finder 
      nTrk = 0;nSeed=0; mSeedFinder->Again(repeat);
      while ((mSeedHelx = mSeedFinder->NextSeed())) 
      {
	nSeed++; nTally++; 
if(BOTOHO) {        
nSits = mSeedFinder->GetHits()->size();
myMask = nSits;
TVector3 myDir(mSeedHelx->Dir()),myPos(mSeedHelx->Pos());
printf("\n\nBOTOHO: Tally=%d Rxy=%g Phi=%g Z=%g Lamda=%g dPsi=%g\n",nTally
      ,myPos.Pt(),myPos.Phi()*k57,myPos[2]
      ,(90-myDir.Theta()*k57),myDir.Angle(myPos)*k57);
}//BOTOHOend        

	if (!mCurrTrak) mCurrTrak = kit->GetTrack();
	mCurrTrak->CutTail();	//Clean track from previous failure

//=============================
	nAdded = FindTrack(0);
//=============================

        mCurrTrak->CutEnds();  	//remove ends without hits
	int ans = 0,fail=13;
//		Refit track   
        int nFitHits = mCurrTrak->GetNHits();
	do {
           if (nAdded<3) {fail = 7; mSeedFinder->FeedBack(0); break;}
          fail = 1; if(nFitHits<3)		break;
{//////////??????????????????????????????
StvNode *myNode = mCurrTrak->GetNode(StvTrack::kFirstPoint);
ptBefRefit = myNode->GetFP().getPt();
StvDebug::Count("PtBefRefit",ptBefRefit);      
nHitsBefRefit = mCurrTrak->GetNHits();
StvDebug::Count("HitsBefRefit",nHitsBefRefit);      
}
//=============================
	  if(mRefit && nFitHits>=mKons->mMinHits) {
	    ans = Refit(1);
if (ans) {
StvDebug::Count("PtFailRefit",ptBefRefit);      
StvDebug::Count("HitsFailRefit",nHitsBefRefit);      
if (nFitHits>15) StvDebug::Break(-1946);
}


	    fail = 2; if (ans) 			break;
            nHits = mCurrTrak->GetNHits();
            myMask += 100*nHits;
            fail = 3; if (nHits<3) 		break;
          }
//=============================
	  nAdded = FindTrack(1);
//=============================
          if (nAdded<=0)			continue;;
          nHits = mCurrTrak->GetNHits();
          fail = 4; if (nHits<mKons->mMinHits) 	break;
// 			few hits added. Refit track to beam again 
//=============================
	  ans = Refit(0);
if (ans) {
StvDebug::Count("PtFailREFIT"  ,ptBefRefit);      
StvDebug::Count("HitsFailREFIT",nHitsBefRefit);      
if (nFitHits>15) StvDebug::Break(-1952);
}

          fail = 5; if (ans) 			break;
          nHits = mCurrTrak->GetNHits();
          myMask += 10000*nHits;

          fail = 6; if (nHits<mKons->mMinHits) 	break;
{//////////??????????????????????????????
StvNode *myNode = mCurrTrak->GetNode(StvTrack::kFirstPoint);
StvDebug::Count("PtAftRefit",myNode->GetFP().getPt());      
StvDebug::Count("HitsAftRefit",mCurrTrak->GetNHits());      
StvDebug::Count("BefHitsAftRefit",nHitsBefRefit);      
}

//=============================

	} while((fail=0));		
if(BOTOHO) {
if (!fail) myMask += 1000000*int(10*mCurrTrak->GetXi2());
printf("BOTOHO: myMask=%d\n",myMask);
return 0;
}//BOTOHOend
      
	nHits = mCurrTrak->GetNHits();
	if (nHits < mKons->mMinHits)	fail+=100;		;
        if (fail) nHits=0;
	if (fail) 	{//Track is failed, release hits & continue
          mSeedFinder->FeedBack(0);
	  mCurrTrak->CutTail();			
	  continue;
        }
	StvNode *node = MakeDcaNode(mCurrTrak); if(node){};

        mSeedFinder->FeedBack(mCurrTrak);

        mCurrTrak->AddId(10*seedFinder+repeat);
        mCurrTrak->SetUsed();
	kit->GetTracks().push_back(mCurrTrak);
	nTrk++;nTrkTot++;

///HISTOS
StvNode *myNode = mCurrTrak->GetNode(StvTrack::kFirstPoint);
if (myNode) {
double Xi2 = mCurrTrak->GetXi2();
double Pt  = myNode->GetFP().getPt();
double Psi = myNode->GetFP().getPsi()		/M_PI*180;
double Lam = atan(myNode->GetFP().getTanL())	/M_PI*180;

StvDebug::Count("Xi2_Pt" ,Pt ,Xi2);
StvDebug::Count("Xi2_Psi",Psi,Xi2);
StvDebug::Count("Xi2_Lam",Lam,Xi2);

StvDebug::Count("nHits_Pt" ,Pt ,nHits);
StvDebug::Count("nHits_Psi",Psi,nHits);
StvDebug::Count("nHits_Lam",Lam,nHits);
StvDebug::Count("Xi2",Xi2);

}
///HISTOSend

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
int mySkip=0,idive = 0,nNode=0,nHits=0,nTotHits=0;
double totLen=0;
StvNode *curNode=0,*preNode=0,*innNode=0,*outNode=0;
const StHitPlane *prevHitPlane=0;

  mHitCounter->Clear();
  mDive->Reset();
  
  if (mCurrTrak->empty()) {//Track empty, Backward tracking, to beam
    StvDebug::ClearGra();
    assert(!idir);
    par[0].set(mSeedHelx); 		//Set seed pars into par[0] and err[0]
    err[0].Set(mSeedHelx); err[0]*= kKalmanErrFact; 
///=
StvDebug::Count("PtSeed",par[0].getPt());/////????????????????????


    par[0].reverse();			//Seed direction OutIn but track direction is allways InOut	
    err[0].Backward();

  } else 	{//Forward or backward tracking
 
    curNode =(idir)? mCurrTrak->back(): mCurrTrak->front();
    par[0] = curNode->GetFP(); err[0] = curNode->GetFE(); 	//Set outer node pars into par[0]
    nTotHits = mCurrTrak->GetNHits();
    mDive->SetPrev(curNode->GetHitPlane()->GetName());	//define previous volume name to avoid doubling
  }

//  	Skip too big curvature or pti
  if (fabs(par[0].getCurv())>mKons->mMaxCurv)	return 0;	
  if (fabs(par[0].getPtin())>mKons->mMaxPti)	return 0;	
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
if(BOTOHO) {
{
  TVector3 myDir(par[0]._d),myPos(par[0]._x);
  printf("BOTOHO.DiveIn: Rxy=%g Phi=%g Z=%g Lam=%g dPsi=%g\n"
        ,myPos.Pt(),myPos.Phi()*k57,myPos[2] 
	, (90-myDir.Theta()*k57),myDir.Angle(myPos)*k57);
}
}//BOTOHOend
    idive = mDive->Dive();
if(BOTOHO) {
{
  TString path(gGeoManager->GetPath());
  printf("BOTOHO Tally %d\tidive=%d \tPath=%s\n",nTally,idive,path.Data());
  TVector3 myDir(par[0]._d),myPos(par[0]._x);
  printf("BOTOHO.DiveOut: Rxy=%g Phi=%g Z=%g Lam=%g dPsi=%g\n"
        ,myPos.Pt(),myPos.Phi()*k57,myPos[2] 
	, (90-myDir.Theta()*k57),myDir.Angle(myPos)*k57);
}
}//BOTOHOend

//+++++++++++++++++++++++++++++++++++++

    double deltaL = mDive->GetLength();
    totLen+=deltaL;
    par[0]=par[1]; err[0]=err[1];			//pars again in par[0]
    if (idive & StvDiver::kDiveBreak) 		break;
    if (idive & StvDiver::kDiveDca  ) 		break;
		// Stop tracking when too big Z or Rxy
    if (fabs(par[0].getZ()) > mKons->mZMax  ) 	break;
    if (par[0].getRxy()     > mKons->mRxyMax) 	break;
    if (fabs(par[0].getCurv())>mKons->mMaxCurv)	break;	
    if (fabs(par[0].getPtin())>mKons->mMaxPti)	break;	

    		
    const StvHits *localHits = 0; 
    if (idive & StvDiver::kDiveHits) {
      float gate[4]={mKons->mCoeWindow,mKons->mCoeWindow
                     ,mKons->mMaxWindow,mKons->mMaxWindow};   

      localHits = mHitter->GetHits(par,err,gate); 
    }


//	Create and add node to myTrak
    preNode = curNode;
    curNode = kit->GetNode();      
    assert(preNode != curNode);
    assert(curNode != mCurrTrak->front());
    assert(curNode != mCurrTrak->back ());

    if (!idir)  {mCurrTrak->push_front(curNode);innNode=curNode;outNode=preNode;}
    else        {mCurrTrak->push_back(curNode) ;innNode=preNode;outNode=curNode;}
    if (outNode){}
    nNode++;		
    if (nNode>256) { //Something very wrong
      Error("FindTrack","Too many nodes =200 Skip track");
      assert (0 && "Too many nodes =256");
      return 0;
    }

    curNode->mLen = (!idir)? totLen:-totLen;
		// Set prediction

    StvELossTrak *eld = mDive->TakeELoss();
    if (eld->TotLen()>0) {
      innNode->SetELoss(eld,idir);
    } else {
      kit->FreeELossTrak(eld); eld = 0;
    }
    curNode->SetXDive(par[0]);
    curNode->SetPre(par[0],err[0],0);
    innNode->SetDer(derivFit,idir);

    if (!localHits)	 continue;	//Never hits in curNode 
    curNode->SetHitPlane(mHitter->GetHitPlane());
    
    if (!localHits->size()) {//No hits in curNode
      mySkip = mHitCounter->AddNit();
      if (mySkip) break;
      continue;
    } 
//??    if (prevHitPlane == mHitter->GetHitPlane()) continue;
    prevHitPlane = mHitter->GetHitPlane();

    fitt->Prep();
    double  minXi2[2]={1e11,1e11},myXi2;
    StvHit *minHit[2]={0};
    minXi2[0] = mKons->mXi2Hit,myXi2=3e33; 
    int minIdx = -1;
    for (int ihit=0;ihit<(int)localHits->size();ihit++) {
      StvHit *hit = (*localHits)[ihit];
StvDebug::AddGra(hit->x()[0]
                ,hit->x()[1]
		,hit->x()[2],1);///???????

      if (hit == prevHit) continue;
      myXi2 = fitt->Xi2(hit);

if(BOTOHO) {  
printf("BOTOHO: %d Xi2=%g Pos=%g %g %g\n",ihit,myXi2,hit->x()[0],hit->x()[1],hit->x()[2]);
}//BOTOHOend

      if (nTotHits > mKons->mMinHits && fitt->IsFailed() == -99) { // Too big track errs
         mySkip = 4; break;	//Track Errors too big, stop tracking
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

    if (mySkip) break; 		//Track Errors too big


    curNode->SetMem(minHit ,minXi2);
    if (minHit[0] ) 		{	// Fit succesful
      assert(minHit[0] != prevHit); 
      prevHit  = minHit[0]; 
      myXi2 = fitt->Xi2(minHit[0]);
      int iuerr = fitt->Update(); 
      if (iuerr<=0 || (nHits<3)) {		//Hit accepted
StvDebug::AddGra(minHit[0]->x()[0]
                ,minHit[0]->x()[1]
		,minHit[0]->x()[2],2);

        mHitCounter->AddHit();
if(BOTOHO) {  
printf("BOTOHO: Added hit Xi2=%g Pos=%g %g %g\n"
      ,myXi2,minHit[0]->x()[0],minHit[0]->x()[1],minHit[0]->x()[2]);
}//BOTOHOend


	nHits++;nTotHits++;assert(nHits<256);
        curNode->SetHE(fitt->GetHitErrs());
        curNode->SetFit(par[1],err[1],0);
        par[0]=par[1];
        err[0]=err[1]; 
static int myDebug = 0;
        if(myDebug) par[0].print("Fitted");
      } else { minHit[0]=0;}
    } else 		{//No Hit or ignored
      myXi2 = 1e11;
      mySkip = mHitCounter->AddNit(); 
    }
    curNode->SetHit(minHit[0]); 
    curNode->SetXi2(myXi2,0);
assert(vsuma(curNode->GetFE(0).TkDir()[0],3*3)>0.1);

    if (mySkip) break;
  } while((idive & StvDiver::kDiveHits) || idive==0); // End Dive&Fitter loop 

  mCurrTrak->SetTypeEnd(mySkip);
  if (!idir) {
    double eff = mHitCounter->Eff(); if (eff){}
StvDebug::Count("Eff_vs_nHits",nHits,eff);
    int myReject = mHitCounter->Reject();
    if (myReject) {

     mCurrTrak->CutTail(); return 0; }
  }

if (StvDebug::Debug()>=3) {
    StvDebug::SetActGra(1);
    StvDebug::ShowGra();
}

  if (nHits>3) {
    double tlen = mCurrTrak->GetLength();
    assert(tlen >0.0 && tlen<1500);
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
  if (eld->TotLen()>0) {
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
  assert(fabs(testDca)<1e-3*(1+dcaNode->mLen));

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
int StvKalmanTrackFinder::Refit(int idir)
{
  return mTrackFitter->Refit(mCurrTrak,idir);
}
