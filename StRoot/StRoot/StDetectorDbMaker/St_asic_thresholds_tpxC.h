#ifndef St_asic_thresholds_tpxC_h
#define St_asic_thresholds_tpxC_h

#include "TChair.h"
#include "tables/St_asic_thresholds_tpx_Table.h"

class St_asic_thresholds_tpxC : public TChair {
 public:
  static St_asic_thresholds_tpxC* 	instance();
  asic_thresholds_tpx_st 	*Struct(Int_t i = 0) 	{return ((St_asic_thresholds_tpx*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	thresh_lo(Int_t i = 0) 	{return Struct(i)->thresh_lo;}
  Int_t 	thresh_hi(Int_t i = 0) 	{return Struct(i)->thresh_hi;}
  Int_t 	n_seq_lo(Int_t i = 0) 	{return Struct(i)->n_seq_lo;}
  Int_t 	n_seq_hi(Int_t i = 0) 	{return Struct(i)->n_seq_hi;}
 protected:
  St_asic_thresholds_tpxC(St_asic_thresholds_tpx *table=0) : TChair(table) {}
  virtual ~St_asic_thresholds_tpxC() {fgInstance = 0;}
 private:
  static St_asic_thresholds_tpxC* fgInstance;
  ClassDefChair(St_asic_thresholds_tpx, asic_thresholds_tpx_st )
  ClassDef(St_asic_thresholds_tpxC,1) //C++ TChair for asic_thresholds_tpx table class
};
#endif
