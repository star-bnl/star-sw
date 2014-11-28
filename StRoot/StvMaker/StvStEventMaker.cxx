
// $Id: StvStEventMaker.cxx,v 1.2 2013/05/24 16:35:00 perev Exp $
/*!
\author V Perev 2030

A maker StvStEventMaker remove all redundant info from StEvent
if StEvent is used for input. Only TpcHits must survive
<br>
*/
#include "StEvent/StEvent.h"
#include "StvStEventMaker.h"
ClassImp(StvStEventMaker)
  
//_____________________________________________________________________________
StvStEventMaker::StvStEventMaker(const char *name) : StMaker(name)
{
}

//_____________________________________________________________________________
Int_t StvStEventMaker::Make()
{

  StEvent *event = (StEvent*)GetInputDS("StEvent");
  if (!event) return kStOK;
  StSPtrVecObject& V = event->content();
  int n = V.size();
  for (int i=0; i<n; i++) {
    StObject *to = V[i];
    if (!to) continue;
    if (strstr(to->ClassName(),"Hit"  )) 	continue;
    if (strstr(to->ClassName(),"Track"))	continue;
    V[i] = 0; delete to;
  }
  return kStOK;
}
//_____________________________________________________________________________
StvStEventMaker* StvStEventMaker::Inst()
{
  StMaker *mk = StMaker::GetChain();
  assert(mk);
// 			Search of StIOMaker::inputStream
  mk = mk->GetMaker("inputStream");
  if (!mk) return 0;
  assert(strcmp(mk->ClassName(),"StIOMaker")==0);
  StvStEventMaker *myMaker = new StvStEventMaker;
  TDataSet *par = myMaker->GetParent();
  par->Remove(myMaker);
  par = mk->GetParent();
  TList* tl= par->GetList();
  tl->AddAfter(mk,myMaker);
  myMaker->SetParent(par);
  return myMaker;
}
