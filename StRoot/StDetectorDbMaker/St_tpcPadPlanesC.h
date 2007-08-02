#ifndef St_tpcPadPlanesC_h
#define St_tpcPadPlanesC_h

#include "TChair.h"
#include "tables/St_tpcPadPlanes_Table.h"

class St_tpcPadPlanesC : public TChair {
 public:
  static St_tpcPadPlanesC* 	instance();
  tpcPadPlanes_st 	*Struct(Int_t i = 0) 	 {return ((St_tpcPadPlanes*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	 {return GetNRows();}
  Int_t 	padRows(Int_t i = 0) 	         {return Struct(i)->padRows;}
  Int_t 	innerPadRows(Int_t i = 0) 	 {return Struct(i)->innerPadRows;}
  Int_t 	innerPadRows48(Int_t i = 0) 	 {return Struct(i)->innerPadRows48;}
  Int_t 	innerPadRows52(Int_t i = 0) 	 {return Struct(i)->innerPadRows52;}
  Int_t 	outerPadRows(Int_t i = 0) 	 {return Struct(i)->outerPadRows;}
  Int_t 	superInnerPadRows(Int_t i = 0) 	 {return Struct(i)->superInnerPadRows;}
  Int_t 	superOuterPadRows(Int_t i = 0) 	 {return Struct(i)->superOuterPadRows;}
  Double_t 	innerSectorPadWidth(Int_t i = 0) {return Struct(i)->innerSectorPadWidth;}
  Double_t 	innerSectorPadLength(Int_t i = 0){return Struct(i)->innerSectorPadLength;}
  Double_t 	innerSectorPadPitch(Int_t i = 0) {return Struct(i)->innerSectorPadPitch;}
  Double_t 	innerSectorRowPitch1(Int_t i = 0){return Struct(i)->innerSectorRowPitch1;}
  Double_t 	innerSectorRowPitch2(Int_t i = 0){return Struct(i)->innerSectorRowPitch2;}
  Double_t 	firstPadRow(Int_t i = 0) 	 {return Struct(i)->firstPadRow;}
  Double_t 	firstOuterSectorPadRow(Int_t i=0){return Struct(i)->firstOuterSectorPadRow;}
  Double_t 	lastOuterSectorPadRow(Int_t i =0){return Struct(i)->lastOuterSectorPadRow;}
  Double_t 	firstRowWidth(Int_t i = 0) 	 {return Struct(i)->firstRowWidth;}
  Double_t 	lastRowWidth(Int_t i = 0) 	 {return Struct(i)->lastRowWidth;}
  Double_t 	outerSectorPadWidth(Int_t i = 0) {return Struct(i)->outerSectorPadWidth;}
  Double_t 	outerSectorPadLength(Int_t i = 0){return Struct(i)->outerSectorPadLength;}
  Double_t 	outerSectorPadPitch(Int_t i = 0) {return Struct(i)->outerSectorPadPitch;}
  Double_t 	outerSectorRowPitch(Int_t i = 0) {return Struct(i)->outerSectorRowPitch;}
  Double_t 	outerSectorLength(Int_t i = 0) 	 {return Struct(i)->outerSectorLength;}
  Double_t 	ioSectorSeparation(Int_t i = 0)  {return Struct(i)->ioSectorSeparation;}
  Double_t 	innerSectorEdge(Int_t i = 0) 	 {return Struct(i)->innerSectorEdge;}
  Double_t 	outerSectorEdge(Int_t i = 0) 	 {return Struct(i)->outerSectorEdge;}
  Double_t 	innerSectorPadPlaneZ(Int_t i = 0){return Struct(i)->innerSectorPadPlaneZ;}
  Double_t 	outerSectorPadPlaneZ(Int_t i = 0){return Struct(i)->outerSectorPadPlaneZ;}
  Int_t* 	innerPadsPerRow(Int_t i = 0) 	 {return Struct(i)->innerPadsPerRow;}
  Int_t* 	outerPadsPerRow(Int_t i = 0) 	 {return Struct(i)->outerPadsPerRow;}
  Double_t* 	innerRowRadii(Int_t i = 0) 	 {return Struct(i)->innerRowRadii;}
  Double_t* 	outerRowRadii(Int_t i = 0) 	 {return Struct(i)->outerRowRadii;}
 protected:
  St_tpcPadPlanesC(St_tpcPadPlanes *table=0) : TChair(table) {}
  virtual ~St_tpcPadPlanesC() {SafeDelete(fgInstance);}
 private:
  static St_tpcPadPlanesC* fgInstance;
  ClassDefChair(St_tpcPadPlanes, tpcPadPlanes_st )
  ClassDef(St_tpcPadPlanesC,1) //C++ TChair for tpcPadPlanes table class
};
#endif
