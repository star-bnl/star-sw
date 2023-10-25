#ifndef St_starTriggerDelayC_h
#define St_starTriggerDelayC_h

#include "TChair.h"
#include "tables/St_starTriggerDelay_Table.h"

class St_starTriggerDelayC : public TChair {
 public:
  static St_starTriggerDelayC* 	instance();
  starTriggerDelay_st 	*Struct(Int_t i = 0) 	const {return ((St_starTriggerDelay*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t 	clocks(Int_t i = 0) 	const {return Struct(i)->clocks;}
  Float_t 	tZero(Int_t i = 0) 	const {return Struct(i)->tZero;}
  Float_t       TrigT0(Int_t i = 0)     const; // usec
  Float_t       TrigT0GG(Int_t io = 0, Int_t i = 0)   const; // usec add cables 
 protected:
  St_starTriggerDelayC(St_starTriggerDelay *table=0) : TChair(table) {}
  virtual ~St_starTriggerDelayC() {if (Table()->IsMarked()) delete GetThisTable(); fgInstance = 0;}
 private:
  static St_starTriggerDelayC* fgInstance;
  ClassDefChair(St_starTriggerDelay, starTriggerDelay_st )
  ClassDef(St_starTriggerDelayC,1) //C++ TChair for starTriggerDelay table class
};
#endif
