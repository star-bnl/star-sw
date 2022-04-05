#ifndef St_tpcAnodeHVavgC_h
#define St_tpcAnodeHVavgC_h

#include "TChair.h"
#include "tables/St_tpcAnodeHVavg_Table.h"

class St_tpcAnodeHVavgC : public TChair {
 public:
  static St_tpcAnodeHVavgC* 	instance();
  tpcAnodeHVavg_st *Struct(Int_t i = 0) 	const {return ((St_tpcAnodeHVavg*) Table())->GetTable()+i;}
  UInt_t     	    getNumRows()                const {return GetNRows();}
  UShort_t          sector(Int_t i = 0) 	const {return Struct(i)->sector;}
  UShort_t          socket(Int_t i = 0) 	const {return Struct(i)->socket;}
  Float_t 	    voltage(Int_t i = 0) 	const;
  Float_t 	    rms(Int_t i = 0) 	        const {return Struct(i)->rms;}
  Int_t 	    numentries(Int_t i = 0) 	const {return Struct(i)->numentries;}
  Int_t 	    numoutliers(Int_t i = 0) 	const {return Struct(i)->numoutliers;}
  Bool_t	    livePadrow(Int_t sec = 1, Int_t padrow = 1) const { return voltagePadrow(sec,padrow) > 500; }
  Float_t	    voltagePadrow(Int_t sec = 1, Int_t padrow = 1) const; // sector=1..24 , padrow=1..100
  Bool_t            tripped(Int_t sec = 1, Int_t padrow = 1)       const;// { return (voltage() < -100); }
 protected:
  St_tpcAnodeHVavgC(St_tpcAnodeHVavg *table=0) : TChair(table) {}
  virtual ~St_tpcAnodeHVavgC() {fgInstance = 0;}
 private:
  static St_tpcAnodeHVavgC* fgInstance;
  ClassDefChair(St_tpcAnodeHVavg, tpcAnodeHVavg_st )
  ClassDef(St_tpcAnodeHVavgC,1) //C++ TChair for tpcAnodeHVavg table class
};
#endif
