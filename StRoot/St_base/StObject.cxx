// $Id: StObject.cxx,v 1.5 1999/12/13 21:40:41 perev Exp $
// $Log: StObject.cxx,v $
// Revision 1.5  1999/12/13 21:40:41  perev
// Remove warnings
//
// Revision 1.4  1999/11/17 14:22:10  perev
// bug in dtor fix
//
// Revision 1.3  1999/11/15 23:09:10  perev
// Streamer for StrArray and auto remove
//
// Revision 1.2  1999/06/23 20:31:04  perev
// StArray I/O + browser
//
// Revision 1.1  1999/04/30 13:15:55  fisyak
// Ad StObject, modification StArray for StRootEvent
//
#include "StObject.h"
#include "TROOT.h"
#include "TBrowser.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TRealData.h"
#include "StArray.h"

//_____________________________________________________________________________
StObject::~StObject()
{
  UInt_t colIdx,objIdx,u;
  u = GetUniqueID();
  if (!u) return;
  StRegistry::Ident(u,colIdx,objIdx);
  if (!colIdx) return;
  StStrArray *ar = StRegistry::GetColl(colIdx);
  if (!ar) return;
  unsigned int n = ar->GetSize();
  if (objIdx>=n)			return;
  if (ar->At(objIdx) != (TObject*)this) return;
  ar->RemoveAt(objIdx);
}
//_____________________________________________________________________________
void StObject::Browse(TBrowser *tb)
{
StObject::Browse(this,tb);
}
//_____________________________________________________________________________
int StObject::Browse(const TObject *This,TBrowser *tb)
{
  int num=0;
  TClass *tc = This->IsA();
  if (!tc) 	return 0;
  const TList *tl = tc->GetListOfRealData();
  if (!tl) tc->BuildRealData(); 
  tl = tc->GetListOfRealData();
  if (!tl) 	return 0;
  TListIter nextMember(tl);
  TRealData *tr = 0; TDataMember *tm=0;
  while ((tr=(TRealData*)nextMember()))
  {
    tm = tr->GetDataMember();
    if (!tm->IsaPointer())	continue;
    if (tm->IsBasic())		continue;
    TClass *cm = gROOT->GetClass(tm->GetTypeName(),1);
    if (!cm) 			continue;
    if (!cm->InheritsFrom(TObject::Class())) 	continue;
    int offset = tr->GetThisOffset();
    if (offset<=0)		continue;
    TObject **our = (TObject **)((const char*)This+offset);
    if (!*our) continue;
    num++; if (!tb) return 1;
    tb->Add(*our);
  }
  return num;
}



ClassImp(StObject);
