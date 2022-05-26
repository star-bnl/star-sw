#ifndef St_itpcDeadFEEC_h
#define St_itpcDeadFEEC_h

#include "TChair.h"
#include "tables/St_itpcDeadFEE_Table.h"

class St_itpcDeadFEEC : public TChair {
 public:
  static St_itpcDeadFEEC* 	instance();
  itpcDeadFEE_st 	*Struct(Int_t i = 0) 	const {return ((St_itpcDeadFEE*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Short_t 	sector(Int_t i = 0) 	const {return Struct(i)->sector;}
  Short_t 	row(Int_t i = 0) 	const {return Struct(i)->row;}
  Short_t 	padMin(Int_t i = 0) 	const {return Struct(i)->padMin;}
  Short_t 	padMax(Int_t i = 0) 	const {return Struct(i)->padMax;}
  Short_t 	FEE(Int_t i = 0) 	const {return Struct(i)->FEE;}
  Short_t 	RDO(Int_t i = 0) 	const {return Struct(i)->RDO;}
 protected:
  St_itpcDeadFEEC(St_itpcDeadFEE *table=0) : TChair(table) {}
  virtual ~St_itpcDeadFEEC() {fgInstance = 0;}
 private:
  static St_itpcDeadFEEC* fgInstance;
  ClassDefChair(St_itpcDeadFEE, itpcDeadFEE_st )
  ClassDef(St_itpcDeadFEEC,1) //C++ TChair for itpcDeadFEE table class
};
#endif
