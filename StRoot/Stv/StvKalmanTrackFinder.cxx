#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"
#include "TCernLib.h"

#include "StvSeedFinder.h"
#include "StvKalmanTrackFinder.h"

#include "StvToolkit.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "StvUtil/StvDebug.h"
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

typedef std::vector<StvNode*> 		StvNodeVec;
typedef std::map<double,StvNode*> 	StvNodeMap;
typedef StvNodeMap::iterator 		StvNodeMapIter ;


class StvRefitPermutator
{
public:
enum {kPermNodes=3, kMaxKont=(1<<kPermNodes)-1};
  StvRefitPermutator(StvTrack *trak,int lane);
  int operator++();
  int Kont() 		{return mKont;}
private:
int mKont;		//Counter for permutation
StvNodeVec mNodes;
StvHit  *mHits[kPermNodes];
};


//______________________________________________________________________________
StvRefitPermutator::StvRefitPermutator(StvTrack *trak,int lane)
{
  mNodes.clear();
  mKont = 0;
  StvNodeMap myMap;
  for (StvNodeIter it=trak->begin(); it!=trak->end();++it) 
  {
    StvNode *node = *it;
    if (!node->GetHit()) 	continue;
    double Xi2 = node->GetXi2(lane);
    if (Xi2>1e5) 		continue;
    myMap[-Xi2] = node;
  }
  int iNode=-1;
  StvNode *node=0, *preNode = 0;
  for (StvNodeMapIter it = myMap.begin();it!=myMap.end();++it) 
  {  
    preNode = node;
    node = (*it).second; iNode++;
    assert(!preNode || preNode->GetXi2(lane)>node->GetXi2(lane));
    mNodes.push_back(node);
    if (iNode>=kPermNodes) continue;
    mHits[iNode]=node->GetHit();
  }
}  
//______________________________________________________________________________
int StvRefitPermutator::operator++()
{
  mKont++;
  int nBadHits = 0;
  if ( mKont < kMaxKont) { // permutations
    for (int jk=0,msk=1; jk<kPermNodes;jk++,msk<<=1) {
      if (jk>=(int)mNodes.size()) return 999;
      StvNode *node=mNodes[jk];
      if ((mKont&msk)==0)	{ node->SetHit(mHits[jk]);} 
      else            		{ node->SetHit(0); nBadHits++;} 
    }
  } else 			{ //No permutations, sequentional hit cancelation
    int jk = mKont-kMaxKont+kPermNodes;
    if (jk>=(int)mNodes.size()) return 999;
    StvNode *node=mNodes[jk];node->SetHit(0);
    nBadHits = jk+1;
  }
  return nBadHits;
}
  
  



//_____________________________________________________________________________
StvKalmanTrackFinder::StvKalmanTrackFinder(const char *name):StvTrackFinder(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
  mDive = new StvDiver("KalmanTrackFinder");
  mDive->Init();
  double rMax,zMin,zMax;
  StTGeoHelper::Inst()->GetHitShape()->Get(zMin,zMax,rMax);
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
	nSeed++;
						  if (sf->DoShow())  sf->Show();
	if (!mCurrTrak) mCurrTrak = kit->GetTrack();
	mCurrTrak->CutTail();	//Clean track from previous failure
	nAdded = FindTrack(0);

	if (!nAdded) 				continue;
{ double tlen = mCurrTrak->GetLength();
  assert(tlen >0.0 && tlen<1000.);
}
	int ans = 0,fail=13;
    //		Refit track   
	do {
	  ans = Refit(1);
	  if (ans) 				break;
	  nAdded = FindTrack(1);
          if (nAdded<=0)			continue;;
{  double tlen = mCurrTrak->GetLength();
  assert(tlen >0.0 && tlen<1000.);}
// 			few hits added. Refit track to beam again 
	  ans = Refit(0);
	  if (ans) 				break;
	} while((fail=0));		
	nHits = mCurrTrak->GetNHits();
	if (nHits < myMinHits)	fail+=100;		;
	if (fail) 	{//Track is failed, release hits & continue
	  mCurrTrak->CutTail();			continue;
        }
{  double tlen = mCurrTrak->GetLength();
  assert(tlen >0.0 && tlen<1000.);}
	StvNode *node = mCurrTrak->GetNode(StvTrack::kDcaPoint);
	if (node) node->UpdateDca();
        if (node && fabs(node->GetFP()._curv) <1./300) {
	  StvTrackFitter::Inst()->Helix(mCurrTrak,16|1);
        }

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
    par[0].set(mSeedHelx,Hz); par[0].reverse();			//Set seed pars into par[0]
    err[0]=par[0].deltaErrs(); 
  } else 	{//Forward or backward tracking
 
    curNode =(idir)? mCurrTrak->back(): mCurrTrak->front();
    par[0] = curNode->GetFP(); err[0] = curNode->GetFE(); 	//Set outer node pars into par[0]
  }

//  	Skip too big curvature
  if (fabs(par[0]._curv)>myConst->mMaxCurv)	return nHits;	

//  	skip P too small
  { double t = par[0]._tanl, pti = par[0]._ptin;	
    if ((t*t+1.)< myConst->mMinP2*pti*pti)	return nHits;
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
      if (mySkip) StvDebug::Count("hitCountSkip",mySkip);
      if (!mySkip) 		continue;	//No Skip, OK
      if (idir)   		break;
      mySkip = hitCount->Reject();
      if (mySkip) StvDebug::Count("hitCountReje",mySkip);
      if (mySkip) 	break;
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

    		
    if (err[0].Check("AfterDive"))		break;
//    assert(idive || !err[0].Check("AfterDive"));
    const StvHits *localHits = 0; 
    if (idive== kDiveHits) {
static float gate[2]={myConst->mMaxWindow,myConst->mMaxWindow};   
      localHits = mHitter->GetHits(par,gate); 
    }


//	Create and add nodemyTrak
    preNode = curNode;
    curNode = kit->GetNode();      
    if (!idir)  {mCurrTrak->push_front(curNode);innNode=curNode;outNode=preNode;}
    else        {mCurrTrak->push_back (curNode);innNode=preNode;outNode=curNode;}
    nNode++;		// assert(nNode<200);
    if (nNode>200) { //Something very wrong
      Error("FindTrack","Too many nodes =200 Skip track");
      return 0;
    }

//    assert(!idive || !par[0].check("FindTrack.1"));
    curNode->mLen = (!idir)? totLen:-totLen;
		// Set prediction
    StvELossData eld = mDive->GetELossData();
    innNode->SetELoss(eld,idir);
    err[0].Add(innNode->mELossData,par[0]);
    curNode->SetPre(par[0],err[0],0);
//    assert(idir || !preNode || innNode->GetFP().getRxy()<outNode->GetFP().getRxy());
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
      if (!iuerr || (nHits<=3)) {		//Hit accepted
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
static const double kEps = 1e-2;
enum {kTryFitMax = 5,kBadHits=5};

//return 0; //??????????????????????????????????????????

  StvNode *node = 0;
  int ans=0,lane = 1;
  StvNode *tstNode = (idir)? mCurrTrak->front(): mCurrTrak->back();
//??  double ptiErr = tstNode->mFE[2].mPP;

  int tryFit = 0,nBadHits=0;
  ans = tkf->Refit(mCurrTrak,idir,lane,1);
//        ==================================

       if (ans>0) { return 130113;}	//Very bad
  else if (ans<=0) { 			//Try to fix
//	Now do helix fit only to find bad hits
//??    ans = tkf->Helix(mCurrTrak,1|2);
//??    if (ans)  	return 130213;
    StvRefitPermutator perm(mCurrTrak,2);


    for (int refIt=0; refIt<55; refIt++)  	//Start iterations
    {
      int nHits = mCurrTrak->GetNHits();
      if (nHits<3) 	return 130313;

      StvNodePars lstPars(tstNode->GetFP());	//Remeber params to compare after refit	
      int anz = tkf->Refit(mCurrTrak,1-idir,1-lane,1); 
  //        ==========================================
      if (anz>0)  	return 130413;


      ans = tkf->Refit(mCurrTrak,idir,lane,1);
  //        ==================================
      if (ans>0)  	return 130513;
      ans+=anz*10000;tryFit++;
      double dif = lstPars.diff(tstNode->GetFP());

      if ( dif < kEps || tryFit > kTryFitMax) {//Tired to try, probably alien hit 
	if (!ans) break;
	nBadHits =  ++perm;  tryFit=0;
	if (nBadHits >kBadHits) return 130613;
      }  

    }
  }
  if (ans) return  130613;
  int nErr = tkf->Clean(mCurrTrak);
  if (nErr && mCurrTrak->GetNHits() >3) { ans = Refit(idir);}		//Recursion
  if (ans) return  130713;

  node = mCurrTrak->GetNode(StvTrack::kDcaPoint);if (!node) return 0;
  node->UpdateDca();
  if (idir==1) {
//??    assert(ptiErr< 4*tstNode->mFE[2].mPP);
  }
  return 0;
}

