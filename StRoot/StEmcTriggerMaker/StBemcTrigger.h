//
// $Id: StBemcTrigger.h,v 1.2 2001/10/12 23:13:55 suaide Exp $
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
    void         GetCrateEtaPatch(Int_t,Int_t*,Int_t*);
    Float_t      GetEtaPatch(Int_t);
    Int_t        GetJetId(Int_t);

  ClassDef(StBemcTrigger, 1) 
};
     
#endif

