/***************************************************************************
 *
 * $Id: THack.cxx,v 1.3 2004/04/07 17:19:20 perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "THack.h"
#include "TClonesArray.h"
#include "TDirectory.h"
#include "TPad.h"
#include "TList.h"
#include "TSystem.h"
#include "TH1.h"

class myClonesArray :public TClonesArray
{
public:
      myClonesArray(){;}
TObject **GetCont(TClonesArray *klon);
TObject **GetKeep(TClonesArray *klon);
};
//______________________________________________________________________________
TObject **myClonesArray::GetCont(TClonesArray *klon)
{
   int offsetC = (char*)&fCont - (char*)this;   
return *((TObject***)((char*)klon+offsetC));
}   

//______________________________________________________________________________
TObject **myClonesArray::GetKeep(TClonesArray *klon)
{
   int offsetK = (char*)&fKeep - (char*)this;   
   TObjArray *k = *((TObjArray**)((char*)klon+offsetK));
   int offsetC = (char*)&fCont - (char*)this;   
   return  *(TObject***)((char*)k+offsetC);
}   

//______________________________________________________________________________
//______________________________________________________________________________
void THack::DeleteClonesArray(TClonesArray *clone)
{
   myClonesArray mycl;

   TObject **keep = mycl.GetKeep(clone);
   TObject **cont = mycl.GetCont(clone);
   int sz = clone->Capacity();
   int i=0;
   if (keep) {
     for (i=0;i<sz;i++) {
       if(!keep[i]) break;
       cont[i]=keep[i];
     }
   }
   delete clone;
}

//______________________________________________________________________________
void THack::ClearClonesArray(TClonesArray *clone)
{
   myClonesArray mycl;

   TObject **keep = mycl.GetKeep(clone);
// TObject **cont = mycl.GetCont(clone); 
   int sz = clone->Capacity();
   int i=0;
   clone->Delete();
   if (keep) {
     TObject *to;
     for (i=0;i<sz;i++) {
       to = keep[i];
       if(!to) break;
       if (to->TestBit(TObject::kNotDeleted)) 	continue;//alive
       clone->New(i);
     }
   }
   clone->Clear();
}

//______________________________________________________________________________
void THack::PadRefresh(TPad *pad,int flag)
{
//  Refresh all TPads in TCanvas recursively

  if (!pad) return;
  pad->Modified();
  pad->Update();
  TList *tl = pad->GetListOfPrimitives();
  if (!tl) return;
  TListIter next(tl);
  TObject *to;
  while ((to=next())) {
    if (to->InheritsFrom(TPad::Class())) PadRefresh((TPad*)to,1);}
  if (flag) return;
  gSystem->ProcessEvents();
}

//______________________________________________________________________________
void THack::HistRelease(TDirectory *dir)
{
//   Release all histograms from TFile(TDirectory)
   if(!dir) return;
   TListIter nextHist(dir->GetList());
//    release histograms from file
   TH1 *h1;
   while ((h1=(TH1*)nextHist())) {
     if (!h1->InheritsFrom(TH1::Class())) continue;
     h1->SetDirectory(0);
   }
}








