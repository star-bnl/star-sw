#ifndef St_pxlControlC_h
#define St_pxlControlC_h

#include "TChair.h"
#include "tables/St_pxlControl_Table.h"

class St_pxlControlC : public TChair {
 public:
  static St_pxlControlC* 	instance();
  pxlControl_st 	*Struct(Int_t i = 0) 	const {return ((St_pxlControl*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UInt_t 	headerToken(Int_t i = 0) 	const {return Struct(i)->headerToken;}
  UInt_t 	separatorToken(Int_t i = 0) 	const {return Struct(i)->separatorToken;}
  UInt_t 	endToken(Int_t i = 0) 	const {return Struct(i)->endToken;}
  UInt_t 	dummyState(Int_t i = 0) 	const {return Struct(i)->dummyState;}
  Float_t 	pixelSize(Int_t i = 0) 	const {return Struct(i)->pixelSize;}
  Float_t 	centerOfDiodeZ(Int_t i = 0) 	const {return Struct(i)->centerOfDiodeZ;}
  Float_t 	centerOfDiodeX(Int_t i = 0) 	const {return Struct(i)->centerOfDiodeX;}
  UChar_t 	headerLength(Int_t i = 0) 	const {return Struct(i)->headerLength;}
  UChar_t 	hardwareIdPosition(Int_t i = 0) 	const {return Struct(i)->hardwareIdPosition;}
  UChar_t 	chipIdStartBit(Int_t i = 0) 	const {return Struct(i)->chipIdStartBit;}
  UChar_t 	chipIdEndBit(Int_t i = 0) 	const {return Struct(i)->chipIdEndBit;}
  UChar_t 	chipIdPow(Int_t i = 0) 	const {return Struct(i)->chipIdPow;}
  UChar_t 	overflowBit(Int_t i = 0) 	const {return Struct(i)->overflowBit;}
  UChar_t 	rowOrColumnFlagBit(Int_t i = 0) 	const {return Struct(i)->rowOrColumnFlagBit;}
  UChar_t 	codingStartBit(Int_t i = 0) 	const {return Struct(i)->codingStartBit;}
  UChar_t 	codingEndBit(Int_t i = 0) 	const {return Struct(i)->codingEndBit;}
  UChar_t 	dataStartBit(Int_t i = 0) 	const {return Struct(i)->dataStartBit;}
  UChar_t 	dataEndBit(Int_t i = 0) 	const {return Struct(i)->dataEndBit;}
  UChar_t 	sensorGoodStatusMin(Int_t i = 0) 	const {return Struct(i)->sensorGoodStatusMin;}
  UChar_t 	sensorGoodStatusMax(Int_t i = 0) 	const {return Struct(i)->sensorGoodStatusMax;}
  UChar_t 	rowColumnGoodStatus(Int_t i = 0) 	const {return Struct(i)->rowColumnGoodStatus;}
 protected:
  St_pxlControlC(St_pxlControl *table=0) : TChair(table) {}
  virtual ~St_pxlControlC() {fgInstance = 0;}
 private:
  static St_pxlControlC* fgInstance;
  ClassDefChair(St_pxlControl, pxlControl_st )
  ClassDef(St_pxlControlC,1) //C++ TChair for pxlControl table class
};
#endif
