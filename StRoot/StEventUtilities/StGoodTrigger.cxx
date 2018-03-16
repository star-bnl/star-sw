#include "StGoodTrigger.h"
#include "TString.h"
#include "TObjString.h"
#include "TObjArray.h"
#include "StMessMgr.h" 
StGoodTrigger *StGoodTrigger::fgStGoodTrigger = 0;
ClassImp(StGoodTrigger);
//________________________________________________________________________________
StGoodTrigger::StGoodTrigger(const Char_t *trigList) {
  fgStGoodTrigger = this;
  fGoodTriggerIds.clear();
  TString Trig(trigList);
  if (Trig == "") return;
  if        (Trig.Contains("y2014",TString::kIgnoreCase)) {
    Trig = 
      "450050, 450060,"         // vpdmb-5-p-nobsmd-hlt
      "450005, 450015, 450025"; // vpdmb-5-p-nobsmd
  } else if (Trig.Contains("y2016",TString::kIgnoreCase)) { 
    Trig = 
      "520001, 520011, 520021, 520031, 520041, 520051," // VPDMB-5-p-sst (2.58B)  - (1 : 4.84M))
      "520802, 520812, 520822, 520832, 520842";         // VPDMB-5-p-hlt (1.81B)  - (43 : 0.55M; 45 : 24.09M) 
  }
  TObjArray *obj = Trig.Tokenize("[^ ;,:]");
  Int_t nParsed = obj->GetEntries();
  for (Int_t k = 0; k < nParsed; k++) {
    if (obj->At(k)) {
      LOG_INFO << "Trigger: " << k << "\t" << ((TObjString *) obj->At(k))->GetName() << endm;
      TString t(((TObjString *) obj->At(k))->GetName());
      Int_t trig = t.Atoi();
      if (! trig) continue;
      fGoodTriggerIds.push_back(trig);
    }
  }
  obj->SetOwner(kFALSE);
  delete obj;
}
//________________________________________________________________________________
Bool_t StGoodTrigger::IsGood(const vector<UInt_t> &triggers) {
  static Int_t debug = 0;
  UInt_t NT = fGoodTriggerIds.size();
  if (debug) {
    LOG_INFO << "StGoodTrigger::IsGood -- no. of requested triggers " << NT << endm;
  }
  if (!NT) return kTRUE; // no trigger selection
  UInt_t nt = triggers.size();
  if (debug) {
    LOG_INFO << "StGoodTrigger::IsGood -- no. of event triggers " << nt << endm;
  }
  if (! nt) return kFALSE; // no trigger information
  Int_t goodTrigger = -1;
  for (UInt_t i = 0; i < NT; i++) {
    for (UInt_t j = 0; j < nt; j++) {
      if (triggers[j] == fGoodTriggerIds[i]) {
	goodTrigger = triggers[j];
	break;
      }
    }
  }
  if (debug) {
    LOG_INFO << "StGoodTrigger::IsGood -- matched trigger " << goodTrigger << endm;
  }
  if (goodTrigger > 0) return kTRUE;
  return kFALSE;
}
//________________________________________________________________________________
// $Log:$
