#ifndef St_pxlRowColumnStatusC_h
#define St_pxlRowColumnStatusC_h

#include "TChair.h"
#include "tables/St_pxlRowColumnStatus_Table.h"
#include "StPxlUtil/StPxlConstants.h"

class St_pxlRowColumnStatusC : public TChair {
 public:
  static St_pxlRowColumnStatusC* 	instance();
  pxlRowColumnStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_pxlRowColumnStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	rows(Int_t i = 0) 	const {return Struct(i)->rows;}
  UChar_t* 	cols(Int_t i = 0) 	const {return Struct(i)->cols;}
  Int_t         rowStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t row) const {
    return (Int_t) (rows()[kNumberOfPxlRowsOnSensor * ((sector - 1) * (kNumberOfPxlSensorsPerLadder * kNumberOfPxlLaddersPerSector) + (ladder - 1) * kNumberOfPxlSensorsPerLadder + (sensor - 1)) + row]);
  }
  Int_t         columnStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t column) const {
    return (Int_t) (cols()[kNumberOfPxlColumnsOnSensor * ((sector - 1) * (kNumberOfPxlSensorsPerLadder * kNumberOfPxlLaddersPerSector) + (ladder - 1) * kNumberOfPxlSensorsPerLadder + (sensor - 1)) + column]);
  }
 protected:
  St_pxlRowColumnStatusC(St_pxlRowColumnStatus *table=0) : TChair(table) {}
  virtual ~St_pxlRowColumnStatusC() {fgInstance = 0;}
 private:
  static St_pxlRowColumnStatusC* fgInstance;
  ClassDefChair(St_pxlRowColumnStatus, pxlRowColumnStatus_st )
  ClassDef(St_pxlRowColumnStatusC,1) //C++ TChair for pxlRowColumnStatus table class
};
#endif
