#ifndef St_tpcMaxHitsC_h
#define St_tpcMaxHitsC_h
#include "TChair.h"
#include "tables/St_tpcMaxHits_Table.h"
class St_tpcMaxHitsC : public TChair {
 public:
  static St_tpcMaxHitsC* 	instance();
  tpcMaxHits_st *Struct(Int_t i = 0) {return ((St_tpcMaxHits*) Table())->GetTable()+i;}
  Int_t maxSectorHits() {return Struct()->maxSectorHits;}
  Int_t maxBinZeroHits() {return Struct()->maxBinZeroHits;}
 protected:
  St_tpcMaxHitsC(St_tpcMaxHits *table=0) : TChair(table) {}
  virtual ~St_tpcMaxHitsC() {fgInstance = 0;}
 private:
  static St_tpcMaxHitsC* fgInstance;
  ClassDef(St_tpcMaxHitsC,1)
};
#endif
