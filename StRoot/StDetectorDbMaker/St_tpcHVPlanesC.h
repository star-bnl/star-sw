#ifndef St_tpcHVPlanesC_h
#define St_tpcHVPlanesC_h
#include "TChair.h"
#include "tables/St_tpcHVPlanes_Table.h"
class St_tpcHVPlanesC : public TChair {
 public:
  static St_tpcHVPlanesC* 	instance();
  tpcHVPlanes_st *Struct(Int_t i = 0) {return ((St_tpcHVPlanes*) Table())->GetTable()+i;}
 protected:
  St_tpcHVPlanesC(St_tpcHVPlanes *table=0) : TChair(table) {}
  virtual ~St_tpcHVPlanesC() {fgInstance = 0;}
 private:
  static St_tpcHVPlanesC* fgInstance;
  ClassDef(St_tpcHVPlanesC,1)
};
#endif
