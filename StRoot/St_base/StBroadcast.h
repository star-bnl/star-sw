/***************************************************************************
 *
 * $Id: StBroadcast.h,v 1.1 2001/11/18 00:59:17 perev Exp $
 *
 * Author: Victor Perev, Jul 2000
 ***************************************************************************
 *
 * Description:
 * Simplified version of StBroadcastoryBroadcast class of Thomas Ullrich
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StBroadcast_h
#define StBroadcast_h

#include "TNamed.h"
//#include "TString.h"

class TList;
class StBroadcastEntry;


class StBroadcast : public TNamed {
public:
    StBroadcast(const char *name="StBroadcast",const char *title="");
   ~StBroadcast();
    
    void Broadcast(const char *subj,const char *info,  const char *author="");
    const char *GetBroadcast(const char *subj,const char *author="*") const;
//  const char *GetBroadcast(const char *subj,const char *&author,Int_t &handle) const;
    void Print(Option_t* opt="") const;
    void ls   (Option_t *opt="") const {Print(opt);}

private:
StBroadcastEntry *GetBroadcastEntry(const char *subj,const char *author) const;
    TList *fList;

ClassDef(StBroadcast,0)
};


#endif
