#ifndef St_pxlSensorStatusC_h
#define St_pxlSensorStatusC_h

#include "TChair.h"
#include "tables/St_pxlSensorStatus_Table.h"

class St_pxlSensorStatusC : public TChair {
 public:
  static St_pxlSensorStatusC* 	instance();
  pxlSensorStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_pxlSensorStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	status(Int_t i = 0) 	const {return Struct(i)->status;}
 protected:
  St_pxlSensorStatusC(St_pxlSensorStatus *table=0) : TChair(table) {}
  virtual ~St_pxlSensorStatusC() {fgInstance = 0;}
 private:
  static St_pxlSensorStatusC* fgInstance;
  ClassDefChair(St_pxlSensorStatus, pxlSensorStatus_st )
  ClassDef(St_pxlSensorStatusC,1) //C++ TChair for pxlSensorStatus table class
};
#endif
