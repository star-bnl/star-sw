//
// $Id: StBemcTrigger.h,v 1.3 2002/01/03 21:41:15 suaide Exp $
//
//    

#ifndef STAR_StBemcTrigger
#define STAR_StBemcTrigger

#include "StEmcTrigger.h" 

class StBemcTrigger : public StEmcTrigger
{  
  public: 
                 StBemcTrigger(Int_t date =20300101, Int_t time = 120000);
    virtual     ~StBemcTrigger();
    void         MakeTrigger();
    void         GetCrateEtaPatch(Int_t,Int_t*,Int_t*);
    Float_t      GetEtaPatch(Int_t);
    Int_t        GetJetId(Int_t);

  ClassDef(StBemcTrigger, 1) 
};
     
#endif

