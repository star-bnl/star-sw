#ifndef St_tofDaqMapC_h
#define St_tofDaqMapC_h

#include "TChair.h"
#include "tables/St_tofDaqMap_Table.h"

class St_tofDaqMapC : public TChair {
 public:
  static St_tofDaqMapC* 	instance();
  tofDaqMap_st 	*Struct(Int_t i = 0) 	const {return ((St_tofDaqMap*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  short* 	MRPC2TDIGChanMap(Int_t i = 0) 	const {return Struct(i)->MRPC2TDIGChanMap;}
  short* 	PMT2TDIGLeChanMap(Int_t i = 0) 	const {return Struct(i)->PMT2TDIGLeChanMap;}
  short* 	PMT2TDIGTeChanMap(Int_t i = 0) 	const {return Struct(i)->PMT2TDIGTeChanMap;}
 protected:
  St_tofDaqMapC(St_tofDaqMap *table=0) : TChair(table) {}
  virtual ~St_tofDaqMapC() {fgInstance = 0;}
 private:
  static St_tofDaqMapC* fgInstance;
  ClassDefChair(St_tofDaqMap, tofDaqMap_st )
  ClassDef(St_tofDaqMapC,1) //C++ TChair for tofDaqMap table class
};
#endif
