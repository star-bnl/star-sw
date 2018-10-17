#ifndef St_tpcRDOMapC_h
#define St_tpcRDOMapC_h

#include "TChair.h"
#include "tables/St_tpcRDOMap_Table.h"

class St_tpcRDOMapC : public TChair {
 public:
  static St_tpcRDOMapC* 	instance();
  tpcRDOMap_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcRDOMap*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            const {return GetNRows();}
  UChar_t 	nrows(Int_t i = 0) 	const {return Struct(i)->nrows;}
  UChar_t 	index(Int_t i = 0) 	const {return Struct(i)->idx;}
  UChar_t 	row(Int_t i = 0) 	const {return Struct(i)->row;}
  UChar_t 	padMin(Int_t i = 0) 	const {return Struct(i)->padMin;}
  UChar_t 	padMax(Int_t i = 0) 	const {return Struct(i)->padMax;}
  UChar_t 	rdoI(Int_t i = 0) 	const {return Struct(i)->rdo;}
  Int_t         rdo(Int_t padrow, Int_t pad = 0) const;
 protected:
  St_tpcRDOMapC(St_tpcRDOMap *table=0) : TChair(table) {}
  virtual ~St_tpcRDOMapC() {fgInstance = 0;}
 private:
  static St_tpcRDOMapC* fgInstance;
  ClassDefChair(St_tpcRDOMap, tpcRDOMap_st )
  ClassDef(St_tpcRDOMapC,1) //C++ TChair for tpcRDOMap table class
};
#endif
