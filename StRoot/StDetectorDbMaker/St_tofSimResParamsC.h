#ifndef St_tofSimResParamsC_h
#define St_tofSimResParamsC_h

#include "TChair.h"
#include "tables/St_tofSimResParams_Table.h"

class St_tofSimResParamsC : public TChair {
 public:
  static St_tofSimResParamsC* 	instance();
  tofSimResParams_st 	*Struct(Int_t i = 0) 	const {return ((St_tofSimResParams*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UShort_t * 	resolution(Int_t i = 0) 	const {return Struct(i)->resolution;}
  UChar_t* 	algoFlag(Int_t i = 0) 	const {return Struct(i)->algoFlag;}
 protected:
  St_tofSimResParamsC(St_tofSimResParams *table=0) : TChair(table) {}
  virtual ~St_tofSimResParamsC() {fgInstance = 0;}
 private:
  static St_tofSimResParamsC* fgInstance;
  ClassDefChair(St_tofSimResParams, tofSimResParams_st )
  ClassDef(St_tofSimResParamsC,1) //C++ TChair for tofSimResParams table class
};
#endif
