#ifndef St_tpcExtraGainCorrectionC_h
#define St_tpcExtraGainCorrectionC_h

#include "TChair.h"
#include "tables/St_tpcExtraGainCorrection_Table.h"

class St_tpcExtraGainCorrectionC : public TChair {
 public:
  static St_tpcExtraGainCorrectionC* 	instance();
  tpcExtraGainCorrection_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcExtraGainCorrection*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t 	idx(Int_t i = 0) 	const {return Struct(i)->idx;}
  UChar_t 	nrows(Int_t i = 0) 	const {return Struct(i)->nrows;}
  Int_t 	runMin(Int_t i = 0) 	const {return Struct(i)->runMin;}
  Int_t 	runMax(Int_t i = 0) 	const {return Struct(i)->runMax;}
  UChar_t 	sector(Int_t i = 0) 	const {return Struct(i)->sector;}
  UChar_t 	row(Int_t i = 0) 	const {return Struct(i)->row;}
  Short_t 	padMin(Int_t i = 0) 	const {return Struct(i)->padMin;}
  Short_t 	padMax(Int_t i = 0) 	const {return Struct(i)->padMax;}
  UChar_t 	RDO(Int_t i = 0) 	const {return Struct(i)->RDO;}
  UChar_t 	FEE(Int_t i = 0) 	const {return Struct(i)->FEE;}
  UChar_t 	status(Int_t i = 0) 	const {return Struct(i)->status;}
 protected:
  St_tpcExtraGainCorrectionC(St_tpcExtraGainCorrection *table=0) : TChair(table) {}
  virtual ~St_tpcExtraGainCorrectionC() {fgInstance = 0;}
 private:
  static St_tpcExtraGainCorrectionC* fgInstance;
  ClassDefChair(St_tpcExtraGainCorrection, tpcExtraGainCorrection_st )
  ClassDef(St_tpcExtraGainCorrectionC,1) //C++ TChair for tpcExtraGainCorrection table class
};
#endif
