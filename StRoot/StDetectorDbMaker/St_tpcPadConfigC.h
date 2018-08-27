#ifndef St_tpcPadConfigC_h
#define St_tpcPadConfigC_h

#include "TChair.h"
#include "tables/St_tpcPadConfig_Table.h"

class St_tpcPadConfigC : public TChair {
 public:
  static St_tpcPadConfigC* 	instance();
  tpcPadConfig_st *Struct(Int_t i=0);
  UInt_t     	   getNumRows();
  UChar_t         *itpc(Int_t i=0) {return (((St_tpcPadConfig*) Table())->GetTable(i))->itpc;}
  UChar_t          iTpc(Int_t sector);				   
  UChar_t          iTPC(Int_t sector) {return iTpc(sector);}
  Int_t 	   padRows(Int_t sector);				   
  Int_t 	   innerPadRows(Int_t sector);			   
  Int_t 	   innerPadRows48(Int_t sector);			   
  Int_t 	   innerPadRows52(Int_t sector);			   
  Int_t 	   outerPadRows(Int_t sector);			   
  Int_t 	   superInnerPadRows(Int_t sector);		   
  Int_t 	   superOuterPadRows(Int_t sector);		   
  Double_t 	   innerSectorPadWidth(Int_t sector);		   
  Double_t 	   innerSectorPadLength(Int_t sector);		   
  Double_t 	   innerSectorPadPitch(Int_t sector);		   
  Double_t 	   innerSectorRowPitch1(Int_t sector);		   
  Double_t 	   innerSectorRowPitch2(Int_t sector);		   
  Double_t 	   firstPadRow(Int_t sector);			   
  Double_t 	   firstOuterSectorPadRow(Int_t sector);		   
  Double_t 	   lastOuterSectorPadRow(Int_t sector);		   
  Double_t 	   firstRowWidth(Int_t sector);			   
  Double_t 	   lastRowWidth(Int_t sector);			   
  Double_t 	   outerSectorPadWidth(Int_t sector);		   
  Double_t 	   outerSectorPadLength(Int_t sector);		   
  Double_t 	   outerSectorPadPitch(Int_t sector);		   
  Double_t 	   outerSectorRowPitch(Int_t sector);		   
  Double_t 	   outerSectorLength(Int_t sector);		   
  Double_t 	   ioSectorSeparation(Int_t sector);		   
  Double_t 	   innerSectorEdge(Int_t sector);			   
  Double_t 	   outerSectorEdge(Int_t sector);			   
  Double_t 	   innerSectorPadPlaneZ(Int_t sector);		   
  Double_t 	   outerSectorPadPlaneZ(Int_t sector);		   
  Int_t* 	   innerPadsPerRow(Int_t sector);			   
  Int_t* 	   outerPadsPerRow(Int_t sector);			   
  Int_t            padsPerRow(Int_t sector, Int_t row = 1);	   
  Double_t* 	   innerRowRadii(Int_t sector);			   
  Double_t* 	   outerRowRadii(Int_t sector);			   
  //               taken from StRItpcPadPlane			   
  Int_t            numberOfRows(Int_t sector);			   
  Int_t            numberOfInnerRows(Int_t sector);		   
  Int_t            numberOfInnerRows48(Int_t sector);		   
  Int_t            numberOfInnerRows52(Int_t sector);		   
  Int_t            numberOfOuterRows(Int_t sector);		   
  Bool_t           isRowInRange(Int_t sector, Int_t row);		   
  Double_t         radialDistanceAtRow(Int_t sector, Int_t row);      
  Int_t            numberOfPadsAtRow(Int_t sector, Int_t row);	   
  Double_t         PadWidthAtRow(Int_t sector, Int_t row);		   
  Double_t 	   PadLengthAtRow(Int_t sector, Int_t row);	   
  Double_t 	   PadPitchAtRow(Int_t sector, Int_t row);		   
  Double_t 	   RowPitchAtRow(Int_t sector, Int_t row);		   
  Int_t            indexForRowPad(Int_t sector, Int_t row, Int_t pad);
  bool             isiTpcSector(Int_t sector) { return iTpc(sector) == 1; }
  bool             isiTpcPadRow(Int_t sector, Int_t row) { return iTpc(sector) && row >= 1 && row <= numberOfInnerRows(sector); }
  bool             isInnerPadRow(Int_t sector, Int_t row) { return row <= numberOfInnerRows(sector); }
  Int_t            IsRowInner(Int_t sector, Int_t row) {return (row <= innerPadRows(sector)) ? 1 : 0;}
 protected:
  St_tpcPadConfigC(St_tpcPadConfig *table=0) : TChair(table) {}
  virtual ~St_tpcPadConfigC() {fgInstance = 0;}
 private:
  static St_tpcPadConfigC* fgInstance;
  ClassDefChair(St_tpcPadConfig, tpcPadConfig_st )
  ClassDef(St_tpcPadConfigC,1) //C++ TChair for tpcPadConfig table class
};
#endif
