#ifndef St_iTPCSurveyC_h
#define St_iTPCSurveyC_h

#include "TChair.h"
#include "tables/St_iTPCSurvey_Table.h"

class St_iTPCSurveyC : public TChair {
 public:
  static St_iTPCSurveyC* 	instance();
  iTPCSurvey_st 	*Struct(Int_t i = 0) 	const {return ((St_iTPCSurvey*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	Id(Int_t i = 0) 	const {return Struct(i)->Id;}
  Float_t 	Angle(Int_t i = 0) 	const {return Struct(i)->Angle;}
  Float_t 	dx(Int_t i = 0) 	const {return Struct(i)->dx;}
  Float_t 	dy(Int_t i = 0) 	const {return Struct(i)->dy;}
  Float_t 	ScaleX(Int_t i = 0) 	const {return Struct(i)->ScaleX;}
  Float_t 	ScaleY(Int_t i = 0) 	const {return Struct(i)->ScaleY;}
  Char_t* 	comment(Int_t i = 0) 	const {return Struct(i)->comment;}
 protected:
  St_iTPCSurveyC(St_iTPCSurvey *table=0) : TChair(table) {}
  virtual ~St_iTPCSurveyC() {fgInstance = 0;}
 private:
  static St_iTPCSurveyC* fgInstance;
  ClassDefChair(St_iTPCSurvey, iTPCSurvey_st )
  ClassDef(St_iTPCSurveyC,1) //C++ TChair for iTPCSurvey table class
};
#endif
