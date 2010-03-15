#ifndef St_tpcRDOMasksC_h
#define St_tpcRDOMasksC_h

#include "TChair.h"
#include "tables/St_tpcRDOMasks_Table.h"

class St_tpcRDOMasksC : public TChair {
 public:
  static St_tpcRDOMasksC* 	instance();
  tpcRDOMasks_st 	*Struct(Int_t i = 0) 	{return ((St_tpcRDOMasks*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  UInt_t 	sector(Int_t i = 0) 	        {return Struct(i)->sector;}
  UInt_t 	mask(Int_t i = 0) 	        {return Struct(i)->mask;}
  UInt_t        getSectorMask(UInt_t sector);
  static UInt_t rdoForPadrow(Int_t row) { //Function returns the rdo board number for a given padrow index. Range of map used is 1-45.
    UInt_t rdo = 0;
    if      (row > 0 && row <=  8) rdo = 1;
    else if (row > 8 && row <= 13) rdo = 2;
    else if (row >13 && row <= 21) rdo = 3;
    else if (row >21 && row <= 29) rdo = 4;
    else if (row >29 && row <= 37) rdo = 5;
    else if (row >37 && row <= 45) rdo = 6;
    return rdo;
  }
  Bool_t        isOn(UInt_t sector,UInt_t rdo)  {    
    if(sector < 1 || sector > 24 || rdo < 1 || rdo > 6)	return 0;
    UInt_t MASK = getSectorMask(sector);
    MASK = MASK >> (rdo - 1);
    MASK &= 0x00000001;
    return MASK;
  }
  Bool_t       isRowOn(UInt_t sector, UInt_t row) {return isOn(sector, rdoForPadrow(row));}
 protected:
  St_tpcRDOMasksC(St_tpcRDOMasks *table=0) : TChair(table) {}
  virtual ~St_tpcRDOMasksC() {fgInstance = 0;}
 private:
  static St_tpcRDOMasksC* fgInstance;
  ClassDefChair(St_tpcRDOMasks, tpcRDOMasks_st )
  ClassDef(St_tpcRDOMasksC,1) //C++ TChair for tpcRDOMasks table class
};
#endif
