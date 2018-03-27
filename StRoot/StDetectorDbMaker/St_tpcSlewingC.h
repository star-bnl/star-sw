#ifndef St_tpcSlewingC_h
#define St_tpcSlewingC_h

#include "TChair.h"
#include "tables/St_tpcSlewing_Table.h"
#include "St_tpcPadConfigC.h"

class St_tpcSlewingC : public TChair {
 public:
  static St_tpcSlewingC* 	instance();
  tpcSlewing_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcSlewing*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t         type(Int_t i = 0)               const {return Struct(i)->type;}
  Int_t         npar(Int_t i = 0)               const {return Struct(i)->npar;}
  Double_t      minT(Int_t i = 0)               const {return Struct(i)->min;}
  Double_t      a(Int_t i = 0, Int_t n = 0)     const {return Struct(i)->a[n];}
  Double_t      slewing(Int_t sector, Int_t i = 0, Double_t q = 0) {
    if (St_tpcPadConfigC::instance()->iTpc(sector)) return 0;
    switch (type(i)) {
      case 1001 : { return func1001(q,a(i,0),a(i,1),a(i,2),a(i,3)); }
      // define other functions as needed
      default   : {}
    }
    return 0;
  }
  Double_t        correctedT(Int_t sector, Int_t padrow, Double_t q, Double_t T) { // T [microsec]
    if (St_tpcPadConfigC::instance()->iTpc(sector)) return T;
    Int_t inout = (St_tpcPadConfigC::instance()->IsRowInner(sector,padrow) ? 0 : 1); // padrow = 1..45
    return T - (T > minT(inout) ? slewing(sector,inout,q) : 0);
  }
 protected:
  St_tpcSlewingC(St_tpcSlewing *table=0) : TChair(table) {}
  virtual ~St_tpcSlewingC() {fgInstance = 0;}
  Double_t func1001(Double_t q, Double_t p0, Double_t p1, Double_t p2, Double_t p3) {
    // An exponential of [1/sqrt(q)]^p3
    return p0 * TMath::Exp(-p1*TMath::Power(q,-0.5*p3)) + p2;
  }
 private:
  static St_tpcSlewingC* fgInstance;
  ClassDefChair(St_tpcSlewing, tpcSlewing_st )
  ClassDef(St_tpcSlewingC,1) //C++ TChair for tpcSlewing table class
};
#endif
