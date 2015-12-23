#ifndef St_istMappingC_h
#define St_istMappingC_h

#include "TChair.h"
#include "tables/St_istMapping_Table.h"

class St_istMappingC : public TChair {
 public:
  static St_istMappingC* 	instance();
  istMapping_st 	*Struct(Int_t i = 0) 	const {return ((St_istMapping*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t* 	mapping(Int_t i = 0) 	const {return Struct(i)->mapping;}
 protected:
  St_istMappingC(St_istMapping *table=0) : TChair(table) {}
  virtual ~St_istMappingC() {fgInstance = 0;}
 private:
  static St_istMappingC* fgInstance;
  ClassDefChair(St_istMapping, istMapping_st )
  ClassDef(St_istMappingC,1) //C++ TChair for istMapping table class
};
#endif
