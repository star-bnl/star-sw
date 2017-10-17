#ifndef __CINT__
#include <stdio.h>
#include "TSystem.h"
#include "TBenchmark.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1.h"
#include "TCanvas.h"
#include "TTreeIter.h"
#endif

TBranch *tb=0;
TFile *muFile =0;
TTree *muTree=0;
TH1F *hz=0;
TH1F *hx=0;
TH1F *ht=0;
TCanvas *c1=0;

void th()
{

  TTreeIter th(muTree);
  th.AddFile("test.MuDst.root");
//  th.ls();
//  return;
  const int *&run = th("MuEvent.mEventInfo.mRunId");
  const int *&evt = th("MuEvent.mEventInfo.mId");
  const int& nGlobs = th("GlobalTracks");
  printf("&run=%p &evt=%p\n",&run,&evt);
  int n=0;
  while (th.Next()) 
  {
  printf("serial = %d run=%d evt=%d nGlo = %d\n",n,*run,*evt,nGlobs);n++;
  }
}
