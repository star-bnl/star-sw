#ifndef __TMVAdata_h__
#define __TMVAdata_h__
#include "TString.h"
#include "TObjString.h"
#include "TObjArray.h"
#include "TCollection.h"
#include <map>
#include "Riostream.h"
#include "PVgadgets.h"
#include "TTableDescriptor.h"
#include "TPRegexp.h"
class TMVAdata {
 public:
/*   TMVAdata(const Char_t *names = "postx:prompt:beam:cross:tof:notof:BEMC:noBEMC:EEMC:noEEMC:nWE:iMc:EMC:noEMC:chi2" */
/* 	   ":xV:yV:zV:xMc:yMc:zMc:zVpd:vR:Rank:noTracks:NoMcTracksWithHits:l:lBest:lMcBest:good:timebucket") : fNames(names) {} */
  virtual ~TMVAdata() {SafeDelete(fTable);}
  static TMVAdata   *instance() {
    if (! fgInstance) {
      fgInstance = new TMVAdata();
    }
    return fgInstance;
  }
  PVgadgets_st *GetArray() {return fTable->GetTable();}
  TTableDescriptor *GetTableDesc() {return fTable->GetRowDescriptors();}
  Bool_t AcceptVar(TString string) {return fAcceptMap[string];}
  void   SetPPV() {fiPPV = kTRUE;}
  void   SetPileUp() {fiPileUp = kTRUE;}
  void   SetAcceptVar(TString string) { fAcceptMap[string] = kTRUE; }
  void   SetListOfActiveVariables(const Char_t *list);
  Bool_t PPV() {return fiPPV;}  
  Bool_t PileUp() {return fiPileUp;}
  void   Init();
  Bool_t AcceptVar(const Char_t *var) {return AcceptVar(TString(var));}
  const Char_t *Names() {return fNames.Data();}
  void Print(); 
 private:
  St_PVgadgets *fTable;
  Bool_t fiPPV, fiPileUp;
  static TMVAdata* fgInstance;
  TMVAdata() : fNames("") {
    fiPPV = kFALSE;
    fiPileUp = kFALSE;
    fTable = new St_PVgadgets("PVgadgets",1); 
    TTableDescriptor *ds = GetTableDesc();
    tableDescriptor_st *s = ds->GetTable();
    for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
      if (i) fNames += ":";
      fNames += s->fColumnName;
    }
  }
  TMVAdata(const TMVAdata&) {}
  TString fNames;
  std::map<TString,Bool_t> fAcceptMap;
  ClassDef(TMVAdata,1)
};
#endif /* __TMVAdata_h__ */
