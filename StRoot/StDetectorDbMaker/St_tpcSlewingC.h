#ifndef St_tpcSlewingC_h
#define St_tpcSlewingC_h

#include "TChair.h"
#include "tables/St_tpcSlewing_Table.h"
#include "St_tpcPadConfigC.h"

class St_tpcSlewingC : public TChair {
 public:
  static St_tpcSlewingC* 	instance();
  tpcSlewing_st 	*Struct(long i = 0) 	const {return ((St_tpcSlewing*) Table())->GetTable()+i;}
  int     	getNumRows()                	const {return GetNRows();}
  long         type(long i = 0)               	const {return Struct(i)->type;}
  long         npar(long i = 0)               	const {return Struct(i)->npar;}
  double      minT(Int_t i = 0)               	const {return Struct(i)->min;}
  double      a(int i = 0, int n = 0)     	const {return Struct(i)->a[n];}
  double      slewing(int sector, int i = 0, double q = 0) {
    if (St_tpcPadConfigC::instance()->iTpc(sector)) return 0;
    switch (type(i)) {
      case 1001 : { return func1001(q,a(i,0),a(i,1),a(i,2),a(i,3)); }
      // define other functions as needed
      default   : {}
    }
    return 0;
  }
  double        correctedT(int sector, int padrow, double q, double T) { // T [microsec]
    if (St_tpcPadConfigC::instance()->iTpc(sector)) return T;
    int inout = (St_tpcPadConfigC::instance()->isInnerPadRow(sector,padrow) ? 0 : 1); // padrow = 1..45
    return T - (T > minT(inout) ? slewing(sector,inout,q) : 0);
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
