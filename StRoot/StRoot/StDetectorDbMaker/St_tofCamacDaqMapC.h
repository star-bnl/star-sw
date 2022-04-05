#ifndef St_tofCamacDaqMapC_h
#define St_tofCamacDaqMapC_h

#include "TChair.h"
#include "tables/St_tofCamacDaqMap_Table.h"

class St_tofCamacDaqMapC : public TChair {
 public:
  static St_tofCamacDaqMapC* 	instance();
  tofCamacDaqMap_st 	*Struct(Int_t i = 0) 	const {return ((St_tofCamacDaqMap*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	entries(Int_t i = 0) 	const {return Struct(i)->entries;}
  short* 	detectorId(Int_t i = 0) 	const {return Struct(i)->detectorId;}
  short* 	trayId(Int_t i = 0) 	const {return Struct(i)->trayId;}
  short* 	daqChannel(Int_t i = 0) 	const {return Struct(i)->daqChannel;}
  short* 	adcChan(Int_t i = 0) 	const {return Struct(i)->adcChan;}
  short* 	tdcChan(Int_t i = 0) 	const {return Struct(i)->tdcChan;}
 protected:
  St_tofCamacDaqMapC(St_tofCamacDaqMap *table=0) : TChair(table) {}
  virtual ~St_tofCamacDaqMapC() {fgInstance = 0;}
 private:
  static St_tofCamacDaqMapC* fgInstance;
  ClassDefChair(St_tofCamacDaqMap, tofCamacDaqMap_st )
  ClassDef(St_tofCamacDaqMapC,1) //C++ TChair for tofCamacDaqMap table class
};
#endif
