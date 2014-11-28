#ifndef St_TpcPadCorrectionC_h
#define St_TpcPadCorrectionC_h

#include "TChair.h"
#include "tables/St_TpcPadCorrection_Table.h"
#include "TF1.h"

class St_TpcPadCorrectionC : public TChair {
 public:
  static St_TpcPadCorrectionC* 	instance();
  TpcPadCorrection_st 	*Struct(Int_t i = 0) 	const {return ((St_TpcPadCorrection*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Char_t* 	HistName(Int_t i = 0) 	        const {return Struct(i)->HistName;}
  Char_t* 	Type(Int_t i = 0) 	        const {return Struct(i)->Type;}	
  Short_t 	npads(Int_t i = 0) 		const {return Struct(i)->npads;}
  Short_t 	InOut(Int_t i = 0) 		const {return Struct(i)->InOut;}
  Short_t 	N(Int_t i = 0) 	                const {return Struct(i)->N;}
  Short_t 	R(Int_t i = 0) 			const {return Struct(i)->R;}
  const Double_t*params(Int_t i = 0)            const {return &Struct(i)->a0;}
  Double_t 	a0(Int_t i = 0) 	        const {return Struct(i)->a0;}  
  Double_t 	a1(Int_t i = 0) 		const {return Struct(i)->a1;}  
  Double_t 	a2(Int_t i = 0) 		const {return Struct(i)->a2;}  
  Double_t 	a3(Int_t i = 0) 		const {return Struct(i)->a3;}  
  Double_t 	a4(Int_t i = 0) 		const {return Struct(i)->a4;}  
  Double_t 	a5(Int_t i = 0) 		const {return Struct(i)->a5;}  
  Double_t 	a6(Int_t i = 0) 		const {return Struct(i)->a6;}  
  Double_t 	a7(Int_t i = 0) 		const {return Struct(i)->a7;}  
  Double_t 	a8(Int_t i = 0) 		const {return Struct(i)->a8;}  
  Double_t 	a9(Int_t i = 0) 		const {return Struct(i)->a9;}  
  Double_t 	prob(Int_t i = 0) 		const {return Struct(i)->prob;}
  //                          InOut = 1/2, No. of pads 1/7; MuOrSigma = 0/1
  Double_t      GetCorrection(Double_t pad, Int_t io=1, Int_t np = 1, Int_t MuOrSigma = 0) {
    if (! fFunc)                          return 0;
    if ((io < 1 || io > 2) ||
	(np < 1 || np > 7) ||
	(MuOrSigma < 0 || MuOrSigma > 1)) return 0;
    Int_t  indx = 2*(7*(io-1) + np-1)+MuOrSigma;
    if (! fFunc[indx])   return 0;
    Int_t ip = TMath::Nint(pad);
    Double_t px = pad - ip;
    return fFunc[indx]->Eval(px);
  }
 protected:
  St_TpcPadCorrectionC(St_TpcPadCorrection *table=0);
  virtual ~St_TpcPadCorrectionC();
 private:
  static St_TpcPadCorrectionC* fgInstance;
  TF1**    fFunc;
  ClassDefChair(St_TpcPadCorrection, TpcPadCorrection_st )
  ClassDef(St_TpcPadCorrectionC,1) //C++ TChair for TpcPadCorrection table class
};
#endif
