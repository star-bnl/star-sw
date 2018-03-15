/***************************************************************************
 *
 * $Id:$
 *
 * Author: Yuri Fisyak, March 2018
 ***************************************************************************
 *
 * Description:
 * Check present of good triggers in the Trigger Collection Ids
 *
 ***************************************************************************
 *
 * $Log:$
 *
 */    
#ifndef StGoodTrigger_hh
#define StGoodTrigger_hh
#include "StTriggerId.h"
#include "StTriggerIdCollection.h"

class StGoodTrigger : public StObject {
 public:
  StGoodTrigger(const Char_t *trigList = "");
  StGoodTrigger(const vector<UInt_t> &trigList) {fGoodTriggerIds = trigList;}
  virtual ~StGoodTrigger() {}
  static StGoodTrigger *instance() {return fgStGoodTrigger;}
  Bool_t IsGood(const vector<UInt_t> &triggers);
  Bool_t IsGood(const StTriggerId  *triggers)          {return (triggers)                        ? IsGood(triggers->triggerIds()) : kFALSE;}
  Bool_t IsGood(const StTriggerIdCollection *triggers) {return (triggers && triggers->nominal()) ? IsGood(triggers->nominal())    : kFALSE;}
 private:
  vector<UInt_t> fGoodTriggerIds;
  static StGoodTrigger *fgStGoodTrigger;
  ClassDef(StGoodTrigger,0)
};

#endif 
