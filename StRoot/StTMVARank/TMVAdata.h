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
  void   Init();
  Bool_t AcceptVar(const Char_t *var) {return AcceptVar(TString(var));}
  const Char_t *Names() {return fNames.Data();}
  void Print() {
    TTableDescriptor *ds = GetTableDesc();
    tableDescriptor_st *s = ds->GetTable();
    for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
      TString aName(s->fColumnName);
      std::cout << "TMVAdata::AcceptVar\t" << aName.Data() << " = " << fAcceptMap[aName] << std::endl;
    }    
  }
 private:
  St_PVgadgets *fTable;
  Bool_t fiPPV;
  static TMVAdata* fgInstance;
  TMVAdata() : fNames("") {
    fiPPV = kFALSE;
    fTable = new St_PVgadgets("PVgadgets",1); 
    TTableDescriptor *ds = GetTableDesc();
    tableDescriptor_st *s = ds->GetTable();
    for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
      if (i) fNames += ":";
      fNames += s->fColumnName;
    }
  }
  TString fNames;
  std::map<TString,Bool_t> fAcceptMap;
};
//________________________________________________________________________________
void TMVAdata::Init() {
  TTableDescriptor *ds = GetTableDesc();
  tableDescriptor_st *s = ds->GetTable();
  for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
    TString aName(s->fColumnName);
    if (fiPPV && aName == "beam" ||
	fiPPV && aName == "chi2" ||
#if 0
	aName == "beam" || // 20
	aName == "tof" ||
	aName == "notof" ||
	aName == "noBEMC" ||
#endif
	aName == "EEMC" || aName == "noEEMC" ||
	aName == "EMC"  || aName == "noEMC"  ||
#if 1
	//  aName == "postx" && ! iPileUp || // for non pileup
	//    iYear == 2011 && (aName == "EEMC" || aName == "noEEMC") ||
	//    iYear == 2011 && (aName == "EMC" || aName == "noEMC") ||
	aName == "chi2" ||
#endif
	aName == "iMc" ||
	aName == "xV" ||
	aName == "yV" ||
	aName == "zV" ||
	aName == "iMc" ||
	aName == "xMc" ||
	aName == "yMc" ||
	aName == "zMc" ||
	aName == "zVpd" ||
	aName == "vR" ||
	aName == "Rank" ||
	aName == "noTracks" ||
	aName == "NoMcTracksWithHits" ||
	aName == "good" ||
	aName == "l" ||
	aName == "lBest" ||
	aName == "lMcBest" ||
	aName == "timebucket") {
      fAcceptMap[aName] = kFALSE;
    }   else {
      fAcceptMap[aName] = kTRUE;
    }
    if ( fAcceptMap[aName] ) 
      std::cout << "TMVAdata::AcceptVar\t" << aName.Data() << " = " << fAcceptMap[aName] << std::endl;
  }
}
#endif /* __TMVAdata_h__ */
