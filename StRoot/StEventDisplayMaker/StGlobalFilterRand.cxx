// $Id: StGlobalFilterRand.cxx,v 1.2 2004/10/19 22:35:28 perev Exp $
#include "TError.h"
#include "TRandom.h"
#include "TSystem.h"
#include "TCL.h"
#include "StGlobalFilterRand.h"
#include "TObjArray.h"
#include "StEventHelper.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#include "THelixTrack.h"
#include "StContainers.h"
#include "StHit.h"
#include "StTrack.h"


ClassImp(StGlobalFilterRand)
//_____________________________________________________________________________
StGlobalFilterRand::StGlobalFilterRand(int BadGood,int maxNum): StGlobalFilterABC("Rand","")
{
  fMode = BadGood; fMaxNum =maxNum;
}
//_____________________________________________________________________________
void StGlobalFilterRand::Filter(TObjArray *eArr,int flag)
{
  static int nKount=0;nKount++;
  int kind,nTot=0,nZel=0,nSel=0,iSel;
  TObject *to;
  StErrorHelper errh;
    
  int n = eArr->GetLast()+1;
  for (int ioj=0;ioj<n-1;ioj++)
  {
    to = eArr->At(ioj);
    if (!to) 		continue;
    kind = StEventHelper::Kind(to);
    if (!(kind&kTRK)) 	continue;
    nTot++;
    StTrack *trk = (StTrack *)to;
    to = eArr->At(ioj+1);
    if (!to) 		continue;
    kind = StEventHelper::Kind(to);
    if (!(kind&kHRR)) 		{(*eArr)[ioj]=0;(*eArr)[ioj+1]=0;continue;}
    StPtrVecHit *hr = (StPtrVecHit *)to;
    int nhits = hr->size();
    if (nhits<10) 		{(*eArr)[ioj]=0;(*eArr)[ioj+1]=0;continue;}
    StTrackHelper th(trk);
    double len = th.GetLength(); if(len){};
    double mom  = th.GetMom().mag();
    if (mom<0.1) 		{(*eArr)[ioj]=0;(*eArr)[ioj+1]=0;continue;}
    nZel++;
    int bad = trk->bad();
    errh.Add(bad);
    iSel  =  bad && (fMode&1);
    iSel |= !bad && (fMode&2);
    if (!iSel) 		{(*eArr)[ioj]=0;(*eArr)[ioj+1]=0;continue;}
    if (bad==21)        {(*eArr)[ioj]=0;(*eArr)[ioj+1]=0;continue;}
//   Only selected tracks
     nSel++;
  }
  eArr->Compress();
  if (!nSel) return;
  n = eArr->GetLast()+1;
  if (n<=1) return;
  double prob = double(fMaxNum)/nSel;
  TRandom rnd;
  for (int ioj=0;ioj<n-1;ioj++)
  {
    to = eArr->At(ioj);
    if (!to) 		continue;
    kind = StEventHelper::Kind(to);
    if (!(kind&kTRK)) 	continue;
    to = eArr->At(ioj+1);
    int ihit=0;
    if (to) {
      kind = StEventHelper::Kind(to);
      ihit = (kind&kHRR);
    }
    if (rnd.Rndm()<prob) continue;
    eArr->AddAt(0,ioj);
    if (ihit) eArr->AddAt(0,ioj+1);
  }
   
   printf("\nStGlobalFilterRand: %d(%d(%d)) tracks selected \n",nSel,nZel,nTot);
   errh.Print("StGlobalFilterRand");
}
