// $Id: StObject.cxx,v 1.9 2000/04/23 01:00:45 perev Exp $
// $Log: StObject.cxx,v $
// Revision 1.9  2000/04/23 01:00:45  perev
// StEvent monolitic I/O
//
// Revision 1.8  2000/04/20 14:24:09  perev
// StArray fixes
//
// Revision 1.7  2000/04/18 02:57:25  perev
// StEvent browse
//
// Revision 1.6  1999/12/21 15:42:58  fine
// remove compilation warning
//
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
#include "StAutoBrowse.h"

ClassImp(StObject)
//_____________________________________________________________________________
StObject::~StObject()
{
  Int_t colIdx,objIdx; UInt_t u;
  u = GetUniqueID();
  if (!u) return;
  StRegistry::Ident(u,colIdx,objIdx);
  if (!colIdx) return;
  StStrArray *ar = StRegistry::GetColl(colIdx);
  if (!ar) return;
  int n = ar->GetSize();
  if (objIdx>=n)			return;
  if (ar->At(objIdx) != (TObject*)this) return;
  ar->RemoveAt(objIdx);
}
//_____________________________________________________________________________
void StObject::Browse(TBrowser *tb)
{
  StAutoBrowse::Browse(this,tb);
}
//_____________________________________________________________________________
Bool_t StObject::IsFolder()
{
  return StAutoBrowse::Browse(this,0);
}
