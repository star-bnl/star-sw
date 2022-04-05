#ifndef St_pxlSensorStatusC_h
#define St_pxlSensorStatusC_h

#include "TChair.h"
#include "tables/St_pxlSensorStatus_Table.h"
#include "StPxlUtil/StPxlConstants.h"

class St_pxlSensorStatusC : public TChair {
 public:
  static St_pxlSensorStatusC* 	instance();
  pxlSensorStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_pxlSensorStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	status(Int_t i = 0) 	const {return Struct(i)->status;}
  Int_t sensorStatus(Int_t sector, Int_t ladder, Int_t sensor) const { ///< 1-9: good or usable status
    return status()[(sector - 1) * kNumberOfPxlLaddersPerSector * kNumberOfPxlSensorsPerLadder + (ladder - 1) * kNumberOfPxlSensorsPerLadder + (sensor - 1)];
  }
 protected:
  St_pxlSensorStatusC(St_pxlSensorStatus *table=0) : TChair(table) {}
  virtual ~St_pxlSensorStatusC() {fgInstance = 0;}
 private:
  static St_pxlSensorStatusC* fgInstance;
  ClassDefChair(St_pxlSensorStatus, pxlSensorStatus_st )
  ClassDef(St_pxlSensorStatusC,1) //C++ TChair for pxlSensorStatus table class
};
#endif
