// $Id: StGlobalFilterTest.cxx,v 1.1 2004/09/28 03:55:23 perev Exp $
#include "TCanvas.h"
#include "TH1F.h"
#include "TCL.h"
#include "StGlobalFilterTest.h"
#include "TObjArray.h"
#include "StEventHelper.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#include "THelixTrack.h"
#include "StContainers.h"
#include "THack.h"


ClassImp(StGlobalFilterTest)
//_____________________________________________________________________________
StGlobalFilterTest::StGlobalFilterTest(): StGlobalFilterABC("Test","Test")
{
   fCanvas = new TCanvas("RESID","Track Residuals",1);
   fHRes[0] = new TH1F("RESH0","Track max resuduals current event",100,0,5.);
   fHRes[1] = new TH1F("RESH1","Track max resuduals all events"   ,100,0,5.);
   fCanvas->Divide(1,2);
   fCanvas->cd(1); fHRes[0]->Draw();
   fCanvas->cd(2); fHRes[1]->Draw();
}
//_____________________________________________________________________________
void StGlobalFilterTest::Filter(TObjArray *eArr,int flag)
{
  int kind;
  TObject *to;
  int n = eArr->GetLast()+1;
  for (int i=0;i<n-1;i++)
  {
    to = eArr->At(i);
    if (!to) continue;
    kind = StEventHelper::Kind(to);
    if (!(kind&kTRK)) continue;
    StTrack *trk = (StTrack *)to;
    to = eArr->At(i+1);
    if (!to) continue;
    kind = StEventHelper::Kind(to);
    if (!(kind&kHRR)) { eArr->AddAt(0,i);continue;}
    StTrackHelper th(trk);
    double len = th.GetLength();
    StPhysicalHelixD *hlx = th.GetHelix();

    StPtrVecHit *hr = (StPtrVecHit *)to;
    int nhits = hr->size();
    if (nhits<10) {(*eArr)[i]=0;(*eArr)[i+1]=0;continue;}
#if 1
    StPhysicalHelixD *outHlx = th.GetHelix(1); 
    THelixTrack myHlx;
    StEventHelper::MyHelix(&myHlx,outHlx);
    myHlx.Backward();
#endif
    
    double maxRes = 0;
    for (int ih=0;ih<nhits;ih++) {
      StHit *hit = hr->at(ih);
      StHitHelper hh(hit);
      if (!hh.IsFit()) continue;
      StThreeVectorD pnt = hh.GetPoint();
      double dist = hlx->distance(pnt);
#if 1
      double myHit[3],myClose[3],myDist[3];
      myHit[0]=pnt.x();
      myHit[1]=pnt.y();
      myHit[2]=pnt.z();
      myHlx.Step(myHit,myClose);
      TCL::vsub(myHit,myClose,myDist,3);
      dist = sqrt(TCL::vdot(myDist,myDist,3));
#endif
      if(dist>maxRes) maxRes = dist;
    }
    if (flag&kHIT){
      fHRes[0]->Fill(maxRes);
      fHRes[1]->Fill(maxRes);
    }
    if (maxRes< 0.8) {(*eArr)[i]=0;(*eArr)[i+1]=0;}
  }
   THack::PadRefresh(fCanvas);

}
//_____________________________________________________________________________
void StGlobalFilterTest::NewEvent(int nrun,int nevt)
{
   fHRes[0]->Reset();
}
