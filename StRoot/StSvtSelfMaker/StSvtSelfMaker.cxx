// $Id $
/// \File StSvtSelfMaker.cxx
/// \author Victor Perev Jan2006
// $Log: StSvtSelfMaker.cxx,v $
// Revision 1.1  2006/02/14 19:02:09  perev
// Svt self alignment maker
//
//
#include <Stiostream.h>
#include <math.h>
#include <string>
#include "TSystem.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "StChain.h"
#include "StBFChain.h"
#include "StMessMgr.h"
#include "SystemOfUnits.h"
#include "StDetectorId.h"
#include "StBFChain.h"
#include "StEventTypes.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiHit.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHitContainer.h"

#include "StSvtSelfMaker.h"
#include "StSelfEvent.h"
#include "StVertexKFit.h"

#include "TFile.h"
#include "TExMap.h"
#include "TObjArray.h"
#include "TMath.h"
#include "TCL.h"
#include "TTree.h"

#include "Sti/StiHitErrorCalculator.h"
#include "StiUtilities/StiDebug.h"
#include "StPhysicalHelixD.hh"
#include "StEventHelper.h"
#include "StTrackGeometry.h"
#include "StTrack.h"
#include "StTrackNode.h"
#include "StGlobalTrack.h"
#include "StPrimaryTrack.h"
#include "StHit.h"
#include "StSvtHit.h"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"

#include "StSsdHit.h"
#include "THelixTrack.h"
#include "vector"

ClassImp(StSvtSelfMaker)
enum {kMinTracks=4};  
int gNHitsTotal=0;

//______________________________________________________________________________
StSvtSelfMaker::StSvtSelfMaker(const Char_t *name) : StMaker(name)
{
  cout <<"StSvtSelfMaker::StSvtSelfMaker() -I- Starting"<<endl;
  mToolkit = StiToolkit::instance();
  mEvent=0;    
  mStStiMap    = new TExMap ;
  mStTrackList = new TObjArray;
  mSelfTrackList = new TObjArray;
  mSelfEvent = 0;
  mSvtGeometry = 0;
}

//______________________________________________________________________________
StSvtSelfMaker::~StSvtSelfMaker() 
{
  cout <<"StSvtSelfMaker::~StSvtSelfMaker() -I- Started/Done"<<endl;
  delete mStStiMap ;
  delete mStTrackList;
  delete mSelfTrackList;
}

//______________________________________________________________________________
void StSvtSelfMaker::Clear(const char*)
{
  mStStiMap->Delete(); 
  mStTrackList->Clear();  
  mSelfTrackList->Delete();  
  mSelfEvent->Clear();
  StMaker::Clear();
}

//______________________________________________________________________________
int StSvtSelfMaker::Finish()
{
  if (!mTTree) return kStWarn;
  TFile *tfile = mTTree->GetCurrentFile(); //just in case we switched to a new file
  tfile->Write();
  mTTree->Print();
  tfile->Close();
  mTTree=0;
  printf("StSvtSelfMaker::Finish() gNHitsTotal=%d\n",gNHitsTotal);
  return StMaker::Finish();
}

//______________________________________________________________________________
int StSvtSelfMaker::Init()
{
  mTreeFile = "test.self.root";
  StBFChain *bfc = dynamic_cast<StBFChain*>(GetChain());
  if (bfc) { //invent TTree file name 
    mTreeFile = bfc->GetFileIn();
    mTreeFile = gSystem->BaseName(mTreeFile);
    int ext = mTreeFile.Index(".");
    if (ext>0) mTreeFile.Replace(ext,999,"");
    mTreeFile +=".self.root";
  }
  mTFile = new TFile(mTreeFile,"RECREATE","TTree SVT Self align ROOT file");
  mTTree = new TTree("SvtSelf","TTree SVT Self align");
  mTTree->SetAutoSave(100000000);  // autosave when 0.1 Gbyte written
  mSelfEvent = new StSelfEvent;
  TBranch *branch = mTTree->Branch("event", mSelfEvent->ClassName(),&mSelfEvent, 16000,99);
  branch->SetAutoDelete(kFALSE);
  return kStOk;
}


//______________________________________________________________________________
int StSvtSelfMaker::InitRun(int run)
{
  TDataSet *ds = GetDataSet("StSvtGeometry");
  assert(ds);
  mSvtGeometry = (StSvtGeometry*)ds->GetObject();
  assert(mSvtGeometry);
  
  return kStOK;
}

//______________________________________________________________________________
int StSvtSelfMaker::Make()
{
  int ans=0;
  mEvent = (StEvent*)GetDataSet("StEvent");
  if (!mEvent) 			return kStWarn;
//  TestVtx(); return 0;
  int nSel = SelectTracks();
  if (nSel<kMinTracks)		return kStWarn;
  MapHits();  
  MakeSelfTracks();  
  ans = MakeVertex();  
  if (ans) 			return kStWarn;
  UpdateSelfTracks();  
  FillEvent();
  return kStOK;
}
//______________________________________________________________________________
int StSvtSelfMaker::SelectTracks()
{
  StVertexHelper vh(mEvent);
  if (!vh.IsValid()) 	return 0;
  TCL::ucopy(&(vh.GetPoint().x()),mVtx,3);
  TCL::ucopy(&(vh.GetPoint().x()),mVtxOld,3);
  TCL::ucopy(  vh.GetErrMtx(),mEtxOld,6);

  StSPtrVecTrackNode& trackNode = mEvent->trackNodes();
  int nTracks = trackNode.size();
  int nTSel=0;
  for (int i=0; i < nTracks; i++) {
    StTrackNode *node = trackNode[i]; if (!node) 	continue;
    StPrimaryTrack *pTrack = (StPrimaryTrack*)(node->track(primary));
    if (!pTrack) 					continue;
    if (!pTrack->numberOfPossiblePoints(kTpcId)) 	continue;
    StTrackHelper th(pTrack);
    if (th.numberOfFitPoints(      ) <20) 		continue;
    if (th.numberOfFitPoints(kSvtId) < 3) 		continue;
    nTSel++;
    mStTrackList->Add(pTrack);
  }
  return nTSel;
}
//______________________________________________________________________________
int StSvtSelfMaker::MapHits()
{

  StiHitContainer *stiHitContainer  =  mToolkit->getHitContainer();
  assert(stiHitContainer);

  std::vector<StiHit*> &hits = stiHitContainer->getHits();
  int nHits = hits.size();
  int nMap=0;
  for (int iHit=0;iHit<nHits; iHit++) {
    StiHit *stiHit = hits[iHit];
    if (stiHit->x() >=60 )	continue;
    if (stiHit->x() <=4  ) 	continue;
    const StiDetector *stiDet = stiHit->detector();
    if (!stiDet) 		continue;
    StHit *stHit = (StHit*)stiHit->stHit();
    if (!stHit)			continue;
    ULong_t hash = TMath::Hash(&stHit,sizeof(void*));
    Long_t& val =(*mStStiMap)(hash, Long_t(stHit));
    assert(!val);
    val = (long)stiHit;
    nMap++;
   }
  return nMap;
}
//______________________________________________________________________________
int StSvtSelfMaker::MakeSelfTracks()  
{
   int nTracks = mStTrackList->GetLast()+1;
   for (int iTrack=0;iTrack<nTracks;iTrack++) {
     StTrack *stTrack = (StTrack*)mStTrackList->At(iTrack);
     StSelfTrack *selfTrack = new StSelfTrack;
     selfTrack->mId = iTrack+1;
     mSelfTrackList->Add(selfTrack);

     StTrackHelper th(stTrack);
     StThreeVectorF oldDir=th.GetMom().unit();
     TCL::ucopy(&(th.GetFirstPoint().x()),selfTrack->mXOld,3);
     TCL::ucopy(&(            oldDir.x()),selfTrack->mDOld,3);
     selfTrack->mCurvOld = th.GetCurv();
     int nHits = th.GetNHits();
     int nSHits = 0;
     for (int iHit=0;iHit<nHits; iHit++) {
       const StHit *stHit = th.GetHit(iHit);
       const StThreeVectorF& pos = stHit->position();
       if (pos.perp2()>60*60)	continue;
       ULong_t hash = TMath::Hash(&stHit,sizeof(void*));
       StiHit *stiHit = (StiHit*)mStStiMap->GetValue(hash,(long)stHit);
       if (!stiHit) 		continue;
       const StiDetector *stiDet = stiHit->detector();
       if (!stiDet) 		continue;
       const StiPlacement *place = stiDet->getPlacement();
       StSelfHit *selfHit = new StSelfHit;
       selfTrack->Add(selfHit);
       selfHit->mHardwarePosition=stHit->hardwarePosition();
       selfHit->mTrackNumber=iTrack+1;
       selfHit->mNormalRefAngle = place->getNormalRefAngle();
       selfHit->mNormalRadius   = place->getNormalRadius();
       selfHit->mNormalYOffset  = place->getNormalYoffset();
       selfHit->mZCenter        = 0;
       TCL::ucopy(&pos.x()      ,selfHit->mXg,3);
       TCL::ucopy(&(stiHit->x()),selfHit->mXl,3);
       int detId = stHit->detector();
       int layer,wafer,ladder,barrel;
       switch (detId) {
         case kSvtId: {
	   StSvtHit *hit = (StSvtHit *)stHit;
           layer =hit->layer() ;wafer =hit->wafer();
	   ladder=hit->ladder();barrel=hit->barrel();
	   selfHit->mId = 1000*layer + 100*wafer + ladder;
           StSvtWaferGeometry* wg =(StSvtWaferGeometry*)mSvtGeometry->getObject(barrel,ladder,wafer);
           selfHit->mZCenter= wg->x(2);

           break;}

	 case kSsdId: {
	   StSsdHit *hit = (StSsdHit *)stHit;
           selfHit->mId = 1000*7            + 100*hit->wafer() + hit->ladder();
           break;}

         default: assert(0);
       }
       
       selfHit->TestIt();
       nSHits++;
     }
     assert(nSHits>=3);
     selfTrack->Fit();
static int OLD=0;
     if (OLD) TCL::ucopy(selfTrack->mXOld,selfTrack->mX,7); 	//????????
//     selfTrack->Print("O");
   } 
   return mSelfTrackList->GetLast()+1;
}
//______________________________________________________________________________
int StSvtSelfMaker::MakeVertex()  
{
  StVertexKFit vkf;
  vkf.SetVtx(mVtx,0);
  vkf.Print("Start");
  int nTrk=mSelfTrackList->GetLast()+1;
  for (int iTrk=0;iTrk<nTrk;iTrk++) {
    StSelfTrack *selfTrack = (StSelfTrack*)mSelfTrackList->At(iTrk);
    vkf.SetTrk(selfTrack->mX,selfTrack->mD,selfTrack->mCurv);
    vkf.Update();
//    vkf.Print();
  }
  vkf.Print("Ended");
  TCL::ucopy(vkf.GetVtx(),mVtx,3);
  TCL::ucopy(vkf.GetEtx(),mEtx,6);
  mChi2 = vkf.GetChi2();
  if (mChi2>10) return 1;
  return 0;
}
//______________________________________________________________________________
int StSvtSelfMaker::UpdateSelfTracks()  
{
  int nTrk=mSelfTrackList->GetLast()+1;
  for (int iTrk=0;iTrk<nTrk;iTrk++) {
    StSelfTrack *selfTrack = (StSelfTrack*)mSelfTrackList->At(iTrk);
    StSelfHit *selfHit = new StSelfHit;
    selfTrack->Add(selfHit);
    selfHit->mHardwarePosition=0;
    selfHit->mTrackNumber=selfTrack->mId;
    selfHit->mNormalRefAngle=atan2(selfTrack->mD[1],selfTrack->mD[0]);			//rotation angle in Sti style
    TCL::ucopy(mVtx,selfHit->mXg,3);
    StThreeVectorD vv(mVtx);
    vv.rotateZ(-selfHit->mNormalRefAngle);
    TCL::ucopy(&(vv.x()),selfHit->mXl,3);
    selfTrack->Fit();
//    selfTrack->Print();
  }
  return 0;
}
//______________________________________________________________________________
int StSvtSelfMaker::FillEvent() 
{
  mSelfEvent->Clear();
  StEvtHddr   *hddr = GetEvtHddr();
  mSelfEvent->mRun  = hddr->GetRunNumber();
  mSelfEvent->mEvt  = hddr->GetEventNumber();
  mSelfEvent->mDate = hddr->GetDateTime();	//DAQ time (GMT)
  mSelfEvent->mChi2 = mChi2;	
  TCL::ucopy(mVtx,mSelfEvent->mVtx,3);
  TCL::ucopy(mEtx,mSelfEvent->mEtx,6);
  TCL::ucopy(mVtxOld,mSelfEvent->mVtxOld,3);
  TCL::ucopy(mEtxOld,mSelfEvent->mEtxOld,6);
  int nTrk=mSelfTrackList->GetLast()+1;
  assert(nTrk>1);
  int iHit=0;
  for (int iTrk=0;iTrk<nTrk;iTrk++) {
    StSelfTrack *selfTrack = (StSelfTrack*)mSelfTrackList->At(iTrk);
    assert(selfTrack);
    TListIter next(&selfTrack->mHits);
    StSelfHit *hit =0;
    while ((hit=(StSelfHit*)next())) {
      if (!hit->mHardwarePosition)	continue;
      hit->TestIt();
      StSelfHit *kHit = (StSelfHit*)mSelfEvent->mHits.New(iHit++);
      *kHit = *hit; gNHitsTotal++;
      assert(fabs(kHit->mXl[1]-hit->mXl[1])<=0);
      kHit->TestIt();
      kHit->Prepare();
    }
  }
  mTTree->Fill();
  return 0;
}
//______________________________________________________________________________
int StSvtSelfMaker::TestVtx()
{
  StVertexHelper vh(mEvent);
  if (!vh.IsValid()) 	return 0;
  TCL::ucopy(&(vh.GetPoint().x()),mVtx,3);
  StVertexKFit vkf;
  vkf.SetVtx(mVtx,0);
  vkf.Print("Start");

  StSPtrVecTrackNode& trackNode = mEvent->trackNodes();
  int nTracks = trackNode.size();
  int nTSel=0;
  for (int i=0; i < nTracks; i++) {
    StTrackNode *node = trackNode[i]; if (!node) 	continue;
    if(!node->track(primary))				continue;
    StTrack *pTrack = (node->track(global));
    if (!pTrack) 					continue;
    if (!pTrack->numberOfPossiblePoints(kTpcId)) 	continue;
    StTrackHelper th(pTrack);
    if (th.numberOfFitPoints(      ) <20) 		continue;
    nTSel++;
    vkf.SetTrk(&(th.GetFirstPoint().x()),&(th.GetMom().unit().x()),th.GetCurv());
    vkf.Update();
  }
  vkf.Print("Ended");
  return nTSel;
}


ClassImp(StSelfTrack)
//______________________________________________________________________________
void StSelfTrack::Add(StSelfHit *shit)
{
  mHits.Add(shit);
}
//______________________________________________________________________________
int StSelfTrack::Fit()
{
  double xyz[10][3];
  int nHits=0;
  TListIter next(&mHits);
  StSelfHit *hit =0,*fstHit=0;
  for (nHits=0;(hit=(StSelfHit*)next());nHits++) {
    TCL::ucopy(hit->mXg,xyz[nHits],3); 
 }

  TCircle circ,cirl;
  double res=circ.Approx(nHits,xyz[0],3);
  assert(res<1);
  double Z0TanL[5];
  circ.FitZ(Z0TanL,nHits,xyz[0],3,xyz[0]+2,3);

  double s=0,xy[2]; 
  double curv = circ.Rho();
  next.Reset();
  for (int iHit=0;(hit=(StSelfHit*)next());iHit++) {
    fstHit=hit;
    TCL::ucopy(hit->mXg,xy,2);
    double ds = circ.Path(xy);
    circ.Move(ds);
    s+=ds;
    StThreeVectorF PosG(hit->mXg[0],hit->mXg[1],hit->mXg[2]);
    StThreeVectorF PosL(hit->mXl[0],hit->mXl[1],hit->mXl[2]);
    StThreeVectorF DirG(circ.Dir()[0],circ.Dir()[1],Z0TanL[1]);
    DirG=DirG.unit();
    TCL::ucopy(&(DirG.x()),hit->mDg,3); 
    cirl = circ;
    double alfa = hit->mNormalRefAngle;
    cirl.Rot(-alfa);
    StThreeVectorF DirL(cirl.Dir()[0],cirl.Dir()[1],Z0TanL[1]);
    DirL=DirL.unit();
    TCL::ucopy(&(DirL.x()),hit->mDl,3); 

    ds = (hit->mNormalRadius-cirl.Pos()[0])/hit->mDl[0];
    StThreeVectorF FitL(cirl.Pos()[0],cirl.Pos()[1],Z0TanL[0]+Z0TanL[1]*s);
    FitL +=ds*DirL;
    TCL::ucopy(&(FitL.x()),hit->mFl,3);
    ds = (hit->mNormalRadius-hit->mXl[0])/hit->mDl[0];
    PosL +=ds*DirL;
    PosG +=ds*DirG;
    TCL::ucopy(&(PosL.x()),hit->mXl,3);
    TCL::ucopy(&(PosG.x()),hit->mXg,3);
    hit->TestIt();
    StThreeVectorF FitG(FitL);
    FitG.rotateZ(alfa);
    TCL::ucopy(&(FitG.x()),hit->mFg,3);
  }   
//  	first hit used to fill track
  mCurv = curv;
  TCL::ucopy(fstHit->mFg,mX,3);
  TCL::ucopy(fstHit->mDg,mD,3);
  return 0;
}
//______________________________________________________________________________
void StSelfTrack::Print(const char *opt) const
{
 if (!opt) opt = "";
 int iOld=0;
 if (strchr(opt,'o')) iOld=1;
 if (strchr(opt,'O')) iOld=1;
 printf("StSelfTrack(%d)",mId);

 printf(" X(");
 for (int i=0;i<3;i++) {
   printf("%g",mX[i]);
   if (iOld) printf("=%g",mXOld[i]);
   printf(" ");
 }
 printf(")");
 printf(" D(");
 for (int i=0;i<3;i++) {
   printf("%g",mD[i]);
   if (iOld) printf("=%g",mDOld[i]);
   printf(" ");
 }
 printf(")");
 
 printf(" C(");
   printf("%g",mCurv);
   if (iOld) printf("=%g",mCurvOld);
   printf(" ");
 printf(")");
 printf("\n");
 
}





