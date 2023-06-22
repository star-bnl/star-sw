#ifndef St_tpcExtraGainCorrectionC_h
#define St_tpcExtraGainCorrectionC_h

#include "TChair.h"
#include "tables/St_tpcExtraGainCorrection_Table.h"

class St_tpcExtraGainCorrectionC : public TChair {
 public:
  static St_tpcExtraGainCorrectionC* 	instance();
  tpcExtraGainCorrection_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcExtraGainCorrection*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Short_t 	idx(Int_t i = 0) 	const {return Struct(i)->idx;}
  Short_t 	nrows(Int_t i = 0) 	const {return Struct(i)->nrows;}
  Int_t 	runMin(Int_t i = 0) 	const {return Struct(i)->runMin;}
  Int_t 	runMax(Int_t i = 0) 	const {return Struct(i)->runMax;}
  Int_t 	sector(Int_t i = 0) 	const {return Struct(i)->sector;}
  Int_t 	row(Int_t i = 0) 	const {return Struct(i)->row;}
  Int_t 	RDO(Int_t i = 0) 	const {return Struct(i)->RDO;}
  Int_t 	FEE(Int_t i = 0) 	const {return Struct(i)->FEE;}
  Short_t 	padMin(Int_t i = 0) 	const {return Struct(i)->padMin;}
  Short_t 	padMax(Int_t i = 0) 	const {return Struct(i)->padMax;}
  Short_t 	status(Int_t i = 0) 	const {return Struct(i)->status;}
 protected:
  St_tpcExtraGainCorrectionC(St_tpcExtraGainCorrection *table=0) : TChair(table) {}
  virtual ~St_tpcExtraGainCorrectionC() {fgInstance = 0;}
 private:
  static St_tpcExtraGainCorrectionC* fgInstance;
  ClassDefChair(St_tpcExtraGainCorrection, tpcExtraGainCorrection_st )
  ClassDef(St_tpcExtraGainCorrectionC,1) //C++ TChair for tpcExtraGainCorrection table class
};
#endif
