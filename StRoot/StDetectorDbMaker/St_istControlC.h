#ifndef St_istControlC_h
#define St_istControlC_h

#include "TChair.h"
#include "tables/St_istControl_Table.h"

class St_istControlC : public TChair {
 public:
  static St_istControlC* 	instance();
  istControl_st 	*Struct(Int_t i = 0) 	const {return ((St_istControl*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t 	kIstChanMaxRmsNoiseLevel(Int_t i = 0) 	const {return Struct(i)->kIstChanMaxRmsNoiseLevel;}
  Float_t 	kIstChanMinRmsNoiseLevel(Int_t i = 0) 	const {return Struct(i)->kIstChanMinRmsNoiseLevel;}
  Float_t 	kIstApvMaxCmNoiseLevel(Int_t i = 0) 	const {return Struct(i)->kIstApvMaxCmNoiseLevel;}
  Float_t 	kIstPedCutDefault(Int_t i = 0) 	const {return Struct(i)->kIstPedCutDefault;}
  Float_t 	kIstHitCutDefault(Int_t i = 0) 	const {return Struct(i)->kIstHitCutDefault;}
  Float_t 	kIstCMNCutDefault(Int_t i = 0) 	const {return Struct(i)->kIstCMNCutDefault;}
  unsigned short 	kIstMinNumOfRawHits(Int_t i = 0) 	const {return Struct(i)->kIstMinNumOfRawHits;}
  unsigned short 	kIstMaxNumOfRawHits(Int_t i = 0) 	const {return Struct(i)->kIstMaxNumOfRawHits;}
  UChar_t 	kIstAlldata(Int_t i = 0) 	const {return Struct(i)->kIstAlldata;}
  UChar_t 	kIstADCdata(Int_t i = 0) 	const {return Struct(i)->kIstADCdata;}
  UChar_t 	kIstZSdata(Int_t i = 0) 	const {return Struct(i)->kIstZSdata;}
  UChar_t 	kIstDefaultTimeBin(Int_t i = 0) 	const {return Struct(i)->kIstDefaultTimeBin;}
  UChar_t 	kIstCurrentTimeBinNum(Int_t i = 0) 	const {return Struct(i)->kIstCurrentTimeBinNum;}
 protected:
  St_istControlC(St_istControl *table=0) : TChair(table) {}
  virtual ~St_istControlC() {fgInstance = 0;}
 private:
  static St_istControlC* fgInstance;
  ClassDefChair(St_istControl, istControl_st )
  ClassDef(St_istControlC,1) //C++ TChair for istControl table class
};
#endif
