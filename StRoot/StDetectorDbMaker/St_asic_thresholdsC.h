#ifndef St_asic_thresholdsC_h
#define St_asic_thresholdsC_h

#include "TChair.h"
#include "tables/St_asic_thresholds_Table.h"

class St_asic_thresholdsC : public TChair {
 public:
  static St_asic_thresholdsC* 	instance();
  asic_thresholds_st 	*Struct(Int_t i = 0) 	{return ((St_asic_thresholds*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	thresh_lo(Int_t i = 0) 	{return Struct(i)->thresh_lo;}
  Int_t 	thresh_hi(Int_t i = 0) 	{return Struct(i)->thresh_hi;}
  Int_t 	n_seq_lo(Int_t i = 0) 	{return Struct(i)->n_seq_lo;}
  Int_t 	n_seq_hi(Int_t i = 0) 	{return Struct(i)->n_seq_hi;}
 protected:
  St_asic_thresholdsC(St_asic_thresholds *table=0) : TChair(table) {}
  virtual ~St_asic_thresholdsC() {fgInstance = 0;}
 private:
  static St_asic_thresholdsC* fgInstance;
  ClassDefChair(St_asic_thresholds, asic_thresholds_st )
  ClassDef(St_asic_thresholdsC,1) //C++ TChair for asic_thresholds table class
};
#endif
