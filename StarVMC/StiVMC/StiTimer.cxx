#include "StiTimer.h"
#include "TList.h"
#include "TNamed.h"
#include "StMessMgr.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


TStopwatch *StiTimer::fgFindTimer=0;
int         StiTimer::fgFindTally=0;
TList      *StiTimer::fgList=0;

class MyHolder : public TNamed
{
public:
   MyHolder(){}
  ~MyHolder(){ delete fSW; }
TStopwatch *fSW;
int        *fTally;
};

//______________________________________________________________________________
void StiTimer::Init(const char *name,TStopwatch *&sw,int &tally)
{
  if (!fgList) {fgList= new TList; fgList->SetOwner();}
  MyHolder *mh = (MyHolder*)fgList->FindObject(name);
  assert(!mh);
  mh = new MyHolder();
  sw = new TStopwatch();
  sw->Stop();
  mh->fSW = sw;
  tally = 0;
  mh->fTally = &tally;
  mh->SetName(name);
  fgList->Add(mh);
}

//______________________________________________________________________________
void StiTimer::Clear(const char *)
{
  delete fgList; fgList=0;
}

//______________________________________________________________________________
void StiTimer::Print(const char *option)
{

 LOG_DEBUG << Form("**** StiTimer::Print ****")<<endm;
 TListIter next(fgList);
 MyHolder *mh=0;
 int n = 0;
 while ((mh=(MyHolder*)next())) {
   n++;
   int tally = mh->fTally[0];
   if (tally<=0) continue;

   double cpu = mh->fSW->CpuTime()/tally;
   double rte = mh->fSW->RealTime()/tally;

   LOG_DEBUG << Form("StiTimer for <%s> Evts =%d CPU/Evts = %g Time/Evts = %g"
         ,mh->GetName(),tally,cpu,rte) << endm;
   mh->fSW->Print("u");

 }
}
 	


		
