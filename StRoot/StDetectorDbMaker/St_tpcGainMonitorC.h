#ifndef St_tpcGainMonitorC_h
#define St_tpcGainMonitorC_h

#include "TChair.h"
#include "tables/St_tpcGainMonitor_Table.h"

class St_tpcGainMonitorC : public TChair {
 public:
  static St_tpcGainMonitorC* 	instance();
  tpcGainMonitor_st 	*Struct(Int_t i = 0) {return ((St_tpcGainMonitor*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                 {return GetNRows();}
  Float_t 	center(Int_t i = 0) 	     {return Struct(i)->center;}
  Float_t 	height(Int_t i = 0)          {return Struct(i)->height;}
  Float_t 	width(Int_t i = 0) 	     {return Struct(i)->width;}
 protected:
  St_tpcGainMonitorC(St_tpcGainMonitor *table=0) : TChair(table) {}
  virtual ~St_tpcGainMonitorC() {fgInstance = 0;}
 private:
  static St_tpcGainMonitorC* fgInstance;
  ClassDefChair(St_tpcGainMonitor, tpcGainMonitor_st )
  ClassDef(St_tpcGainMonitorC,1) //C++ TChair for tpcGainMonitor table class
};
#endif
