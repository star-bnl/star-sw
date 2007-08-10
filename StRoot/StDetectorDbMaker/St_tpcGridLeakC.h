#ifndef St_tpcGridLeakC_h
#define St_tpcGridLeakC_h

#include "TChair.h"
#include "tables/St_tpcGridLeak_Table.h"

enum StGLpos {
  kGLinner=0,
  kGLmiddl=1,
  kGLouter=2
};
class St_tpcGridLeakC : public TChair {
 public:
  static St_tpcGridLeakC* 	instance();
  tpcGridLeak_st 	*Struct(Int_t i = 0) 	{return ((St_tpcGridLeak*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Double_t 	InnerGLRadius(Int_t i = 0) 	{return Struct(i)->InnerGLRadius;}
  Double_t 	MiddlGLRadius(Int_t i = 0) 	{return Struct(i)->MiddlGLRadius;}
  Double_t 	OuterGLRadius(Int_t i = 0) 	{return Struct(i)->OuterGLRadius;}
  Double_t 	InnerGLWidth(Int_t i = 0) 	{return Struct(i)->InnerGLWidth;}
  Double_t 	MiddlGLWidth(Int_t i = 0) 	{return Struct(i)->MiddlGLWidth;}
  Double_t 	OuterGLWidth(Int_t i = 0) 	{return Struct(i)->OuterGLWidth;}
  Double_t 	InnerGLStrength(Int_t i = 0) 	{return Struct(i)->InnerGLStrength;}
  Double_t 	MiddlGLStrength(Int_t i = 0) 	{return Struct(i)->MiddlGLStrength;}
  Double_t 	OuterGLStrength(Int_t i = 0) 	{return Struct(i)->OuterGLStrength;}
  Double_t      getGridLeakStrength(StGLpos pos){
    switch (pos) {
      case (kGLinner) : return InnerGLStrength();
      case (kGLmiddl) : return MiddlGLStrength();
      case (kGLouter) : return OuterGLStrength();
    }
    return 0;
  }
  Double_t      getGridLeakRadius(StGLpos pos) {
    switch (pos) {
      case (kGLinner) : return InnerGLRadius();
      case (kGLmiddl) : return MiddlGLRadius();
      case (kGLouter) : return OuterGLRadius();
    }
    return 0;
  }
  Double_t      getGridLeakWidth(StGLpos pos) {
    switch (pos) {
      case (kGLinner) : return InnerGLWidth();
      case (kGLmiddl) : return MiddlGLWidth();
      case (kGLouter) : return OuterGLWidth();
    }
    return 0;
  }
 protected:
  St_tpcGridLeakC(St_tpcGridLeak *table=0) : TChair(table) {}
  virtual ~St_tpcGridLeakC() {fgInstance = 0;}
 private:
  static St_tpcGridLeakC* fgInstance;
  ClassDefChair(St_tpcGridLeak, tpcGridLeak_st )
  ClassDef(St_tpcGridLeakC,1) //C++ TChair for tpcGridLeak table class
};
#endif
