//
// $Id: StBemcTrigger.h,v 1.1 2001/05/24 14:42:10 suaide Exp $
//
//    

#ifndef STAR_StBemcTrigger
#define STAR_StBemcTrigger

#include "StEmcTrigger.h" 

class StBemcTrigger : public StEmcTrigger
{
  public: 
                 StBemcTrigger();
    virtual     ~StBemcTrigger();
    void         MakeTrigger();
    void         GetModuleEtaPatch(Int_t,Int_t*,Int_t*);
    Int_t        GetPatchId(Int_t,Int_t);
    Float_t      GetEtaPatch(Int_t);
    Int_t        GetJetId(Int_t);
    Int_t        GetModuleId(Int_t);

  ClassDef(StBemcTrigger, 1) 
};
     
#endif

