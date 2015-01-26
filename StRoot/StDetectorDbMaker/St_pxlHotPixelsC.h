#ifndef St_pxlHotPixelsC_h
#define St_pxlHotPixelsC_h

#include "TChair.h"
#include "tables/St_pxlHotPixels_Table.h"

class St_pxlHotPixelsC : public TChair {
 public:
  static St_pxlHotPixelsC* 	instance();
  pxlHotPixels_st 	*Struct(Int_t i = 0) 	const {return ((St_pxlHotPixels*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UInt_t* 	hotPixel(Int_t i = 0) 	const {return Struct(i)->hotPixel;}
 protected:
  St_pxlHotPixelsC(St_pxlHotPixels *table=0) : TChair(table) {}
  virtual ~St_pxlHotPixelsC() {fgInstance = 0;}
 private:
  static St_pxlHotPixelsC* fgInstance;
  ClassDefChair(St_pxlHotPixels, pxlHotPixels_st )
  ClassDef(St_pxlHotPixelsC,1) //C++ TChair for pxlHotPixels table class
};
#endif
