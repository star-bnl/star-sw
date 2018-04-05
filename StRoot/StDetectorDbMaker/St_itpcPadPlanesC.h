#ifndef St_itpcPadPlanesC_h
#define St_itpcPadPlanesC_h

#include "TChair.h"
#include "tables/St_itpcPadPlanes_Table.h"

class St_itpcPadPlanesC : public TChair {
 public:
  static St_itpcPadPlanesC* 	instance();
  itpcPadPlanes_st 	*Struct(Int_t i = 0) 	 {return ((St_itpcPadPlanes*) Table())->GetTable()+i;}
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
  Int_t         padsPerRow(Int_t row = 1)        {return (row <= innerPadRows()) ? 
      innerPadsPerRow()[row-1] : 
      outerPadsPerRow()[row-1-innerPadRows()];}
  Double_t* 	innerRowRadii(Int_t i = 0) 	 {return Struct(i)->innerRowRadii;}
  Double_t* 	outerRowRadii(Int_t i = 0) 	 {return Struct(i)->outerRowRadii;}
  // taken from StRItpcPadPlane
  Int_t         numberOfRows()                   {return padRows();}
  Int_t         numberOfInnerRows()              {return innerPadRows();}
  Int_t         numberOfInnerRows48()            {return innerPadRows48();}
  Int_t         numberOfInnerRows52()            {return innerPadRows52();}
  Int_t         numberOfOuterRows()              {return outerPadRows();}
  Bool_t        isRowInRange(Int_t row)          {return (row >= 1 && row<=numberOfRows()) ? kTRUE: kFALSE;}
  Double_t      radialDistanceAtRow(Int_t row)       {
    if (! isRowInRange(row)) return 0;
    if ( row<=numberOfInnerRows() ) return innerRowRadii()[row-1];
    else                            return outerRowRadii()[row-1-numberOfInnerRows()];
  }
  Int_t   numberOfPadsAtRow(Int_t row)       {
    if (! isRowInRange(row)) return 0;
    if ( row<=numberOfInnerRows() ) return innerPadsPerRow()[row-1];
    return outerPadsPerRow()[row-1-numberOfInnerRows()];
  }
  Double_t PadWidthAtRow(Int_t row)       {
    if (! isRowInRange(row)) return 0;
    if ( row<=numberOfInnerRows()) return innerSectorPadWidth();
    return outerSectorPadWidth();
  }
  Double_t PadLengthAtRow(Int_t row)       {
    if (! isRowInRange(row)) return 0;
    if ( row<=numberOfInnerRows()) return innerSectorPadLength();
    return outerSectorPadLength();
  }
  Double_t PadPitchAtRow(Int_t row)       {
    if (! isRowInRange(row)) return 0;
    if ( row<=numberOfInnerRows()) return innerSectorPadPitch();
    return outerSectorPadPitch();
  }
  Double_t RowPitchAtRow(Int_t row)       {
    if (! isRowInRange(row)) return 0;
    if ( row<=numberOfInnerRows48() ) return innerSectorRowPitch1();
    else if (row>numberOfInnerRows48()&&row<=numberOfInnerRows()) return innerSectorRowPitch2();
    return outerSectorRowPitch();
  }
  Int_t indexForRowPad(Int_t row, Int_t pad)       {
    if (pad >numberOfPadsAtRow(row)) return -1;
    Int_t index = 0;
    if (row>0 && row<=numberOfInnerRows() )             for (Int_t i=1;i<row;i++) index += numberOfPadsAtRow(i);    
    else 
      if (row>numberOfInnerRows()&&row<=numberOfRows()) for (Int_t i=numberOfInnerRows()+1;i<row;i++)  index += numberOfPadsAtRow(i);
    index+=pad-1;
    return index;
  }
 protected:
  St_itpcPadPlanesC(St_itpcPadPlanes *table=0) : TChair(table) {}
  virtual ~St_itpcPadPlanesC() {fgInstance = 0;}
 private:
  static St_itpcPadPlanesC* fgInstance;
  ClassDefChair(St_itpcPadPlanes, itpcPadPlanes_st )
  ClassDef(St_itpcPadPlanesC,1) //C++ TChair for itpcPadPlanes table class
};
#endif
