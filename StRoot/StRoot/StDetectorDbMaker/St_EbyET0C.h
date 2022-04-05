#ifndef St_EbyET0C_h
#define St_EbyET0C_h
#include "TChair.h"
#include "tables/St_EbyET0_Table.h"

class St_EbyET0C : public TChair {
 public:
  static St_EbyET0C* 	instance();
  EbyET0_st 	*Struct(Int_t i = 0) 	const {return ((St_EbyET0*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            const {return GetNRows();}
  Int_t 	detector(Int_t i = 0) 	const {return Struct(i)->detector;}
  Double_t 	min(Int_t i = 0) 	const {return Struct(i)->min;}
  Double_t 	max(Int_t i = 0) 	const {return Struct(i)->max;}
  Int_t 	func(Int_t i = 0) 	const {return Struct(i)->func;}
  Double_t* 	par(Int_t i = 0)        const {return Struct(i)->par;}
  Double_t 	time(Int_t i, Double_t x);
  Double_t 	T0(Int_t i, Double_t x) {return time(i,x);}
 protected:
  St_EbyET0C(St_EbyET0 *table=0) : TChair(table) {}
  virtual ~St_EbyET0C() {fgInstance = 0;}
 private:
  static St_EbyET0C* fgInstance;

  ClassDefChair(St_EbyET0, EbyET0_st )
  ClassDef(St_EbyET0C,1) //C++ TChair for EbyET0 table class
};
#endif

