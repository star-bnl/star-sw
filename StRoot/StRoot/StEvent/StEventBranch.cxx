/***************************************************************************
 *
 * $Id: StEventBranch.cxx,v 2.4 2004/01/30 20:24:03 perev Exp $
 *
 * Author: Victor Perev, May 2001
 ***************************************************************************
 *
 * Description:
 *
 * Do not touch anything here unless you really know what you are doing.
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StEvent.h"
#include "StEventBranch.h"
#include "TObjectSet.h"
#include "TError.h"
#include "TDataSetIter.h"
//_____________________________________________________________________________

ClassImp(StEventBranch)
//_____________________________________________________________________________
StEventBranch::StEventBranch(const char *brName, StEvent *evt,UInt_t tally)
: StXRef(brName,evt, tally)
{
}
//_____________________________________________________________________________
void StEventBranch::AddKlass(const char* className)
{
   assert(FindByName(className)==0);
   TObjectSet *to = new TObjectSet(className,0,0);
   Add(to);
}
//_____________________________________________________________________________
void StEventBranch::Synchro(int toMain)
{
   TDataSetIter next(this);

   TObjectSet *os;
   TDataSet   *ds;
   TObject *to;
   StObject *so;
   StEvent *ste = (StEvent*)GetMain();
   assert(ste);
   StSPtrVecObject  &cnt = ste->content();
   int icl,cntSize = cnt.size();
   while ((ds=next())) {// set loop
     if (ds->IsA() != TObjectSet::Class())		continue;
     const char *className = ds->GetName();
     if (strncmp("St",className,2)!=0)			continue;
     os = (TObjectSet*)ds;
     for (icl=0;icl<cntSize;icl++) {//contLoop
       if (!(so = cnt[icl])) 				continue;
       if (strcmp(className,so->ClassName())!=0)	continue;
       if (so->IsZombie())				continue;
       break;
     }//end contLoop
     if (icl>=cntSize) so = 0;
     os->DoOwner(0);
     if (toMain) {
       if (so) cnt.erase(cnt.begin()+icl);
       to = os->GetObject();
       if(to) cnt.StObjArray::push_back((StObject*)to);
       os->SetObject(0,0);
     } else {
       os->SetObject(so,0);
     }
   } // set loop
}

//_____________________________________________________________________________
StXRefMain *StEventBranch::MakeMain()
{
   StEvent *e = new StEvent();
   return e;
}
//_____________________________________________________________________________
void StEventBranch::Streamer(TBuffer &buf)
{
   StXRef::Streamer(buf);
}
