// $Id: StObject.cxx,v 1.14 2000/09/30 17:48:27 perev Exp $
// $Log: StObject.cxx,v $
// Revision 1.14  2000/09/30 17:48:27  perev
// Zombies cons and loop for stru vector
//
// Revision 1.13  2000/09/15 15:11:58  perev
// Zombie for StEvent
//
// Revision 1.12  2000/07/30 01:49:03  perev
// StObject vers restored
//
// Revision 1.10  2000/06/19 01:28:26  perev
// STL StEvent
//
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
}
//_____________________________________________________________________________
void StObject::Browse(TBrowser *tb)
{
  StAutoBrowse::Browse(this,tb);
}
//_____________________________________________________________________________
Bool_t StObject::IsFolder() const
{
  return StAutoBrowse::Browse((TObject*)this,0);
}
//______________________________________________________________________________
void StObject::Streamer(TBuffer &R__b)
{
//	Stream an object of class StObject.
  unsigned char uc=0;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion();
    uc = 1; if (R__v >= 2) R__b >> uc;
    if (uc) TObject::Streamer(R__b);

  } else {
    R__b.WriteVersion(StObject::Class());
    uc = (GetUniqueID() || TestBit(0xffffffff));
    R__b << uc;
    if (uc) TObject::Streamer(R__b);
  }
}

//______________________________________________________________________________
//void StObject::ShowMembers(TMemberInspector &R__insp, char *R__parent)
//{
//  TObject::ShowMembers(R__insp, R__parent);
//}
