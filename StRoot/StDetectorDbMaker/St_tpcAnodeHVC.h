#ifndef St_tpcAnodeHVC_h
#define St_tpcAnodeHVC_h

#include "TChair.h"
#include "tables/St_tpcAnodeHV_Table.h"

class St_tpcAnodeHVC : public TChair {
 public:
  static St_tpcAnodeHVC* 	instance();
  tpcAnodeHV_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcAnodeHV*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  unsigned short 	sector(Int_t i = 0) 	const {return Struct(i)->sector;}
  unsigned short 	socket(Int_t i = 0) 	const {return Struct(i)->socket;}
  Float_t 	voltage(Int_t i = 0) 	const {return Struct(i)->voltage;}
  bool		livePadrow(int sector = 1, int padrow = 1) { return voltagePadrow(sector,padrow) > 500; }
  float		voltagePadrow(int sector = 1, int padrow = 1); // sector=1..24 , padrow=1..45
  bool tripped() { return (voltage() < -100); }
 protected:
  St_tpcAnodeHVC(St_tpcAnodeHV *table=0) : TChair(table) {}
  virtual ~St_tpcAnodeHVC() {fgInstance = 0;}
 private:
  static St_tpcAnodeHVC* fgInstance;
  ClassDefChair(St_tpcAnodeHV, tpcAnodeHV_st )
  ClassDef(St_tpcAnodeHVC,1) //C++ TChair for tpcAnodeHV table class
};
#endif
