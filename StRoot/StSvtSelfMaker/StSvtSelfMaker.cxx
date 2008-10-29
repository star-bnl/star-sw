// $Id $
/// \File StSvtSelfMaker.cxx
/// \author Victor Perev Jan2006
// $Log: StSvtSelfMaker.cxx,v $
// Revision 1.3  2008/10/29 18:55:18  perev
// Last version
//
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
#include "StMessMgr.h"
#include "SystemOfUnits.h"
#include "StDetectorId.h"
#include "StEventTypes.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiHit.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHitContainer.h"

#include "StSvtSelfMaker.h"
#include "StiUtilities/StSelfEvent.h"
#include "StiUtilities/StVertexKFit.h"

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
  mTFile = GetTFile();
  mTFile->cd();
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
  TCL::ucopy(mVtx,mVtxOld,3);
  TCL::ucopy(  vh.GetErrMtx(),mEtx,6);
  TCL::ucopy(  mEtx,mEtxOld,6);

  StSPtrVecTrackNode& trackNode = mEvent->trackNodes();
  int nTracks = trackNode.size();
  int nTSel=0,nTPri=0;
  for (int i=0; i < nTracks; i++) {
    StTrackNode *node = trackNode[i]; if (!node) 	continue;
    StPrimaryTrack *pTrack = (StPrimaryTrack*)(node->track(primary));
    if (!pTrack) 					continue;
    if (!pTrack->numberOfPossiblePoints(kTpcId)) 	continue;
    StTrackHelper th(pTrack);
    if (th.numberOfFitPoints(      ) <20) 		continue;
    nTPri++;
    if (th.numberOfFitPoints(kSvtId) < 3) 		continue;
    nTSel++;
    mStTrackList->Add(pTrack);
  }
  mSelfEvent->mNPrk = nTPri;
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
     StSelfTrack *selfTrack = new StSelfTrack(1);
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
//       TCL::ucopy(&pos.x()      ,selfHit->mXg,3);

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
       selfHit->SetXl(&(stiHit->x()));
       selfHit->SetGlobal();
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
  vkf.SetVtx(mVtx,mEtx);
  vkf.Print("Start");
  int nTrk=mSelfTrackList->GetLast()+1;
  for (int iTrk=0;iTrk<nTrk;iTrk++) {
    StSelfTrack *selfTrack = (StSelfTrack*)mSelfTrackList->At(iTrk);
    vkf.AddTrk(selfTrack->mX,selfTrack->mD,selfTrack->mCurv,selfTrack->mErr);
//    vkf.Print();
  }
  mChi2 = vkf.Fit();
  if (vkf.GetNFit() <5) return 1;
  if (mChi2>33) 	return 2;

  int discarded = 0;
  for (int iTrk=0;iTrk<nTrk;iTrk++) {
    if (vkf.IsUsed(iTrk)) continue;
    discarded++;
    (*mSelfTrackList)[iTrk] = 0;
  }
  if (discarded) mSelfTrackList->Compress();

  vkf.Print("Ended");
  TCL::ucopy(vkf.GetVtx(),mVtx,3);
  TCL::ucopy(vkf.GetEtx(),mEtx,6);
  return 0;
}
//______________________________________________________________________________
int StSvtSelfMaker::UpdateSelfTracks()  
{
  int nTrk=mSelfTrackList->GetLast()+1;
  for (int iTrk=0;iTrk<nTrk;iTrk++) {
    StSelfTrack *selfTrack = (StSelfTrack*)mSelfTrackList->At(iTrk);
    selfTrack->Fit(mVtx,mEtx);
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
  mSelfEvent->mNTrk = nTrk;	
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
      double kXl[4],hXl[4];
      kHit->GetXl(kXl);
      hit->GetXl(hXl);
      assert(fabs(kXl[1]-hXl[1])<=0);
      kHit->TestIt();
    }
  }
  mTTree->Fill();
  return 0;
}
#if 0
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
    vkf.AddTrk(&(th.GetFirstPoint().x()),&(th.GetMom().unit().x()),th.GetCurv());
  }
  vkf.Fit();
  vkf.Print("Ended");
  return nTSel;
}
#endif//0




