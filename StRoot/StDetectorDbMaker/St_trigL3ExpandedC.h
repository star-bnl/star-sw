#ifndef St_trigL3ExpandedC_h
#define St_trigL3ExpandedC_h

#include "TChair.h"
#include "tables/St_trigL3Expanded_Table.h"

class St_trigL3ExpandedC : public TChair {
 public:
  static St_trigL3ExpandedC* 	instance();
  trigL3Expanded_st 	*Struct(Int_t i = 0) 	 {return ((St_trigL3Expanded*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	 {return GetNRows();}
  Int_t 	runNumber(Int_t i = 0) 	         {return Struct(i)->runNumber;}
  Char_t* 	l2TriggerResultType(Int_t i = 0) {return Struct(i)->l2TriggerResultType;}
  UChar_t* 	name(Int_t i = 0) 	         {return Struct(i)->name;}
  Int_t 	l3TrgId(Int_t i = 0) 	         {return Struct(i)->l3TrgId;}
  Int_t 	l3ExpandedTrgId(Int_t i = 0) 	 {return Struct(i)->l3ExpandedTrgId;}
  Int_t 	l2Algo(Int_t i = 0) 	         {return Struct(i)->l2Algo;}
  Float_t 	l2Ps(Int_t i = 0) 	         {return Struct(i)->l2Ps;}
 protected:
  St_trigL3ExpandedC(St_trigL3Expanded *table=0) : TChair(table) {}
  virtual ~St_trigL3ExpandedC() {if (Table()->IsMarked()) delete GetThisTable(); fgInstance = 0;}
 private:
  static St_trigL3ExpandedC* fgInstance;
  ClassDefChair(St_trigL3Expanded, trigL3Expanded_st )
  ClassDef(St_trigL3ExpandedC,1) //C++ TChair for trigL3Expanded table class
};
#endif
