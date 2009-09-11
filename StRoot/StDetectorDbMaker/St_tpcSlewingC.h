#ifndef St_tpcSlewingC_h
#define St_tpcSlewingC_h

#include "TChair.h"
#include "tables/St_tpcSlewing_Table.h"

class St_tpcSlewingC : public TChair {
 public:
  static St_tpcSlewingC* 	instance();
  tpcSlewing_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcSlewing*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  long          type(Int_t i = 0)               const {return Struct(i)->type;}
  long          npar(Int_t i = 0)               const {return Struct(i)->npar;}
  double        minT(Int_t i = 0)               const {return Struct(i)->min;}
  double        a(Int_t i = 0, Int_t n = 0)     const {return Struct(i)->a[n];}
  double        slewing(Int_t i = 0, double q = 0) {
    switch (type(i)) {
      case 1001 : { return func1001(q,a(i,0),a(i,1),a(i,2),a(i,3)); }
      // define other functions as needed
      default   : {}
    }
    return 0;
  }
  double        correctedT(int padrow = 1, double q = 0, double T = 0) { // T [microsec]
    int inout = (padrow <= 13 ? 0 : 1); // padrow = 1..45
    return T - (T > minT(inout) ? slewing(inout,q) : 0);
  }
 protected:
  St_tpcSlewingC(St_tpcSlewing *table=0) : TChair(table) {}
  virtual ~St_tpcSlewingC() {fgInstance = 0;}
  double func1001(double q, double p0, double p1, double p2, double p3) {
    // An exponential of [1/sqrt(q)]^p3
    return p0 * TMath::Exp(-p1*TMath::Power(q,-0.5*p3)) + p2;
  }
 private:
  static St_tpcSlewingC* fgInstance;
  ClassDefChair(St_tpcSlewing, tpcSlewing_st )
  ClassDef(St_tpcSlewingC,1) //C++ TChair for tpcSlewing table class
};
#endif
