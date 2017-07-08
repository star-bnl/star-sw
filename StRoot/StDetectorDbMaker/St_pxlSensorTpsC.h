#ifndef St_pxlSensorTpsC_h
#define St_pxlSensorTpsC_h

#include "TChair.h"
#include "tables/St_pxlSensorTps_Table.h"

class St_pxlSensorTpsC : public TChair {
 public:
  static St_pxlSensorTpsC* 	instance();
  pxlSensorTps_st 	*Struct(Int_t i = 0) 	const {return ((St_pxlSensorTps*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	Id(Int_t i = 0) 	const {return Struct(i)->Id;}
  UShort_t  	nMeasurements(Int_t i = 0) 	const {return Struct(i)->nMeasurements;}
  Float_t* 	A(Int_t i = 0) 	const {return Struct(i)->A;}
  Float_t* 	X(Int_t i = 0) 	const {return Struct(i)->X;}
  Float_t* 	Y(Int_t i = 0) 	const {return Struct(i)->Y;}
  Float_t* 	W(Int_t i = 0) 	const {return Struct(i)->W;}
 protected:
  St_pxlSensorTpsC(St_pxlSensorTps *table=0) : TChair(table) {}
  virtual ~St_pxlSensorTpsC() {fgInstance = 0;}
 private:
  static St_pxlSensorTpsC* fgInstance;
  ClassDefChair(St_pxlSensorTps, pxlSensorTps_st )
  ClassDef(St_pxlSensorTpsC,1) //C++ TChair for pxlSensorTps table class
};
#endif
