#ifndef St_tpcSectorPositionC_h
#define St_tpcSectorPositionC_h
#include <assert.h>
#include "TChair.h"
#include "tables/St_tpcSectorPosition_Table.h"

class St_tpcSectorPositionC : public TObject {
 public:
  static St_tpcSectorPositionC* 	instance();
  tpcSectorPosition_st 	*Struct(Int_t i = 0) 	        const {assert(i >=0 && i <= 23); return fgTables[i]->GetTable();}
  Float_t 	innerSectorLocalxShift(Int_t i = 0) 	const {return Struct(i)->innerSectorLocalxShift;}
  Float_t 	innerSectorLocalyShift(Int_t i = 0) 	const {return Struct(i)->innerSectorLocalyShift;}
  Float_t 	innerSectorRotationAngle(Int_t i = 0) 	const {return Struct(i)->innerSectorRotationAngle;}
  Float_t 	innerSectorCovMatrix(Int_t i = 0) 	const {return Struct(i)->innerSectorCovMatrix;}
  Float_t 	outerSectorLocalxShift(Int_t i = 0) 	const {return Struct(i)->outerSectorLocalxShift;}
  Float_t 	outerSectorLocalyShift(Int_t i = 0) 	const {return Struct(i)->outerSectorLocalyShift;}
  Float_t 	outerSectorRotationAngle(Int_t i = 0) 	const {return Struct(i)->outerSectorRotationAngle;}
  Float_t 	outerSectorCovMatrix(Int_t i = 0) 	const {return Struct(i)->outerSectorCovMatrix;}

  Double_t      innerPositionOffsetX(Int_t i = 0)       const { return innerSectorLocalxShift(i);}  
  Double_t      outerPositionOffsetX(Int_t i = 0) 	const { return outerSectorLocalxShift(i);}  
  Double_t      innerRotation(Int_t i = 0)        	const { return innerSectorRotationAngle(i);}
  Double_t      outerRotation(Int_t i = 0)        	const { return outerSectorRotationAngle(i);}

 protected:
  St_tpcSectorPositionC() : TObject() {}
  virtual ~St_tpcSectorPositionC() {fgInstance = 0;}
 private:
  static St_tpcSectorPositionC* fgInstance;
  static St_tpcSectorPosition *fgTables[24];
  ClassDef(St_tpcSectorPositionC,1) //C++ TChair for tpcSectorPosition table class
};
#endif
