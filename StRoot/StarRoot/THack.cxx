/***************************************************************************
 *
 * $Id: THack.cxx,v 1.6 2009/05/22 23:49:16 fine Exp $
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
#include "TTree.h"
#include "TError.h"

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

//______________________________________________________________________________
int THack::LineToD(const char *line, const char **lend,
                   int nItems, double *Items,TString *Names)
{
  int nIt=0;
  const char *l=line; char *ll,*le=0;	
  while	((l = strstr(l,"="))) {
    l++;
    if(nIt>nItems) 	break;
    if(Names)Names[nIt]="";
    double d = strtod(l,&ll);
    if (l==ll) 	continue;
    le=ll;
    Items[nIt]=d;
    nIt++;
    if (!Names) continue;
    int n=0;
    for (int jj=-2;ll+jj>=line;jj--) {
      int space = isspace(l[jj]);
      if (space && !n) continue;
      if (!strchr("_()[]",l[jj]) && !isalnum(l[jj])) break;
      Names[nIt-1].Insert(0,l+jj,1);n++;
    }
  }  
  if (lend) *lend=le;
  return nIt;
}

bool THack::IsTreeWritable(const TTree *tree, bool fatal)
{
   // Test whether the TFile assocoated wit TTree is writable
   bool out = false;
   TDirectory *d = 0;
   if (tree && (d = tree->GetDirectory())  && d->IsWritable() ) {
      out = true;
   } else if (tree && fatal) {
      Fatal("IsTreeWritable", "TTree %p %s can not be written", tree,tree->GetName());
   }
   return out;
}


