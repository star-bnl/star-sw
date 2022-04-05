#ifndef St_tpcAnodeHVC_h
#define St_tpcAnodeHVC_h

#include "TChair.h"
#include "tables/St_tpcAnodeHV_Table.h"

class St_tpcAnodeHVC : public TChair {
 public:
  static St_tpcAnodeHVC* 	instance();
  tpcAnodeHV_st *Struct(Int_t i = 0) 	const {return ((St_tpcAnodeHV*) Table())->GetTable()+i;}
  UInt_t     	 getNumRows()           const {return GetNRows();}
  UShort_t 	 sector(Int_t i = 0) 	const {return Struct(i)->sector;}
  UShort_t 	 socket(Int_t i = 0) 	const {return Struct(i)->socket;}
  Float_t 	 voltage(Int_t i = 0) 	const;
  Bool_t	 livePadrow(Int_t sector = 1, Int_t padrow = 1) const { return voltagePadrow(sector,padrow) > 500; }
  Float_t	 voltagePadrow(Int_t sector = 1, Int_t padrow = 1) const ; // sector=1..24 , padrow=1..100
  Bool_t         tripped(Int_t sector = 1, Int_t padrow = 1) const { return (voltagePadrow(sector,padrow) < -100); }
  static  void   sockets(Int_t sector, Int_t padrow, Int_t &e1, Int_t &e2, Float_t &f2);
 protected:
  St_tpcAnodeHVC(St_tpcAnodeHV *table=0) : TChair(table) {}
  virtual ~St_tpcAnodeHVC() {fgInstance = 0;}
 private:
  static St_tpcAnodeHVC* fgInstance;
  ClassDefChair(St_tpcAnodeHV, tpcAnodeHV_st )
  ClassDef(St_tpcAnodeHVC,1) //C++ TChair for tpcAnodeHV table class
};
#endif
