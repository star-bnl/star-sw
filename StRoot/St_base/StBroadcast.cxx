/***************************************************************************
 *
 * $Id: StBroadcast.cxx,v 1.1 2001/11/18 00:59:17 perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "StBroadcast.h"

class TBuffer;
//______________________________________________________________________________
class StBroadcastEntry : public TNamed {
public:
    StBroadcastEntry(const char *subj,const char *info,  const char *author)
   :TNamed(subj,info),fAuth(author){}
   ~StBroadcastEntry(){;}
    	const char *GetSubj() const {return GetName ();}
    	const char *GetInfo() const {return GetTitle();}
    	const char *GetAuth() const {return fAuth.Data();}
virtual	void Print(Option_t* opt="") const;
virtual	void ls   (Option_t *opt="") const {Print(opt);}

private:
    TString fAuth;

ClassDef(StBroadcastEntry,0)
};
//______________________________________________________________________________
//______________________________________________________________________________
ClassImp(StBroadcastEntry)

//______________________________________________________________________________
void StBroadcastEntry::Print(Option_t* opt) const
{
   printf("  %s.%s = %s\n",GetSubj(),GetAuth(),GetInfo());
}
//______________________________________________________________________________
void StBroadcastEntry::Streamer(TBuffer& b) 
{;}

//______________________________________________________________________________
void StBroadcastEntry::ShowMembers(TMemberInspector &, char *)
{}


//______________________________________________________________________________
//______________________________________________________________________________
ClassImp(StBroadcast)
//______________________________________________________________________________
StBroadcast::StBroadcast(const char *name,const char *title):TNamed(name,title)
{
  fList = new TList;
}
//______________________________________________________________________________
StBroadcast::~StBroadcast()
{
  fList->Delete(); delete fList;
}
//______________________________________________________________________________
void StBroadcast::Broadcast(const char *subj,const char *info,const char *author)
{

  StBroadcastEntry *ent = GetBroadcastEntry(subj,author);
  if (ent) {fList->Remove(ent); delete ent;}
  ent = new StBroadcastEntry(subj,info,author);
  fList->AddFirst(ent);
}
//______________________________________________________________________________
StBroadcastEntry *StBroadcast::GetBroadcastEntry(const char *subj,const char *author) const
{
   TListIter next(fList);
   StBroadcastEntry  *ent;
   while ((ent=(StBroadcastEntry*)next())) {
     if (strcmp(subj,ent->GetSubj())!=0)	continue;
     if (author[0]!='*' && 
         strcmp(author,ent->GetAuth())!=0)	continue;
     return ent;
   }
   return 0;
}
//______________________________________________________________________________
const char *StBroadcast::GetBroadcast(const char *subj,const char *author) const
{
  StBroadcastEntry *ent = GetBroadcastEntry(subj,author);
  if (!ent) return 0;
  return ent->GetInfo();
}
//______________________________________________________________________________
void StBroadcast::Print(char const *opt) const
{
  printf("  StBroadcast::%s(%s)\n",GetName(),GetTitle());
  fList->Print(opt);
}











