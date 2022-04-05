#ifndef St_tpcWirePlanesC_h
#define St_tpcWirePlanesC_h

#include "TChair.h"
#include "tables/St_tpcWirePlanes_Table.h"

class St_tpcWirePlanesC : public TChair {
 public:
  static St_tpcWirePlanesC* 	instance();
  tpcWirePlanes_st 	*Struct(Int_t i = 0) 	            {return ((St_tpcWirePlanes*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	            {return GetNRows();}
  Double_t 	anodeWireRadius(Int_t i = 0) 	            {return Struct(i)->anodeWireRadius;}
  Double_t 	frischGridWireRadius(Int_t i = 0) 	    {return Struct(i)->frischGridWireRadius;}
  Double_t 	gatingGridWireRadius(Int_t i = 0) 	    {return Struct(i)->gatingGridWireRadius;}
  Double_t 	anodeWirePitch(Int_t i = 0) 	            {return Struct(i)->anodeWirePitch;}
  Double_t 	frischGridWirePitch(Int_t i = 0) 	    {return Struct(i)->frischGridWirePitch;}
  Double_t 	gatingGridWirePitch(Int_t i = 0) 	    {return Struct(i)->gatingGridWirePitch;}
  Double_t 	innerSectorAnodeWirePadSep(Int_t i = 0)     {return Struct(i)->innerSectorAnodeWirePadSep;}
  Double_t 	innerSectorFrischGridPadSep(Int_t i = 0)    {return Struct(i)->innerSectorFrischGridPadSep;}
  Double_t 	innerSectorGatingGridPadSep(Int_t i = 0)    {return Struct(i)->innerSectorGatingGridPadSep;}
  Double_t 	outerSectorAnodeWirePadSep(Int_t i = 0)     {return Struct(i)->outerSectorAnodeWirePadSep;}
  Double_t 	outerSectorFrischGridPadSep(Int_t i = 0)    {return Struct(i)->outerSectorFrischGridPadSep;}
  Double_t 	outerSectorGatingGridPadSep(Int_t i = 0)    {return Struct(i)->outerSectorGatingGridPadSep;}
  Int_t 	numInnerSectorAnodeWires(Int_t i = 0) 	    {return Struct(i)->numInnerSectorAnodeWires;}
  Int_t 	numInnerSectorFrischGridWires(Int_t i = 0)  {return Struct(i)->numInnerSectorFrischGridWires;}
  Int_t 	numInnerSectorGatingGridWires(Int_t i = 0)  {return Struct(i)->numInnerSectorGatingGridWires;}
  Double_t 	firstInnerSectorAnodeWire(Int_t i = 0) 	    {return Struct(i)->firstInnerSectorAnodeWire;}
  Double_t 	firstInnerSectorFrischGridWire(Int_t i = 0) {return Struct(i)->firstInnerSectorFrischGridWire;}
  Double_t 	firstInnerSectorGatingGridWire(Int_t i = 0) {return Struct(i)->firstInnerSectorGatingGridWire;}
  Double_t 	lastInnerSectorAnodeWire(Int_t i = 0) 	    {return Struct(i)->lastInnerSectorAnodeWire;}
  Int_t 	numOuterSectorAnodeWires(Int_t i = 0) 	    {return Struct(i)->numOuterSectorAnodeWires;}
  Int_t 	numOuterSectorFrischGridWires(Int_t i = 0)  {return Struct(i)->numOuterSectorFrischGridWires;}
  Int_t 	numOuterSectorGatingGridWires(Int_t i = 0)  {return Struct(i)->numOuterSectorGatingGridWires;}
  Double_t 	firstOuterSectorAnodeWire(Int_t i = 0) 	    {return Struct(i)->firstOuterSectorAnodeWire;}
  Double_t 	firstOuterSectorFrischGridWire(Int_t i = 0) {return Struct(i)->firstOuterSectorFrischGridWire;}
  Double_t 	firstOuterSectorGatingGridWire(Int_t i = 0) {return Struct(i)->firstOuterSectorGatingGridWire;}
  Double_t 	lastOuterSectorAnodeWire(Int_t i = 0) 	    {return Struct(i)->lastOuterSectorAnodeWire;}

  Double_t      gateWireRadius(Int_t i = 0)  {return gatingGridWireRadius(i);}				 
  Double_t  	frischGridPitch(Int_t i = 0) {return frischGridWirePitch(i);}				 
  Double_t  	gatePitch(Int_t i = 0)       {return gatingGridWirePitch(i);}				 
  		                                                                                             
  Double_t  	innerSectorAnodeWirePadPlaneSeparation(Int_t i = 0)  {return innerSectorAnodeWirePadSep(i);} 
  Double_t  	innerSectorFrischGridPadPlaneSeparation(Int_t i = 0) {return innerSectorFrischGridPadSep(i);}
  Double_t  	innerSectorGatingGridPadPlaneSeparation(Int_t i = 0) {return innerSectorGatingGridPadSep(i);}
  Double_t  	outerSectorAnodeWirePadPlaneSeparation(Int_t i = 0)  {return outerSectorAnodeWirePadSep(i);} 
  Double_t  	outerSectorFrischGridPadPlaneSeparation(Int_t i = 0) {return outerSectorFrischGridPadSep(i);}
  Double_t  	outerSectorGatingGridPadPlaneSeparation(Int_t i = 0) {return outerSectorGatingGridPadSep(i);}

  Int_t         numberOfInnerSectorAnodeWires(Int_t i = 0)      {return numInnerSectorAnodeWires(i);}	    
  Int_t   	numberOfInnerSectorFrischGridWires(Int_t i = 0) {return numInnerSectorFrischGridWires(i);}
  Int_t   	numberOfInnerSectorGatingGridWires(Int_t i = 0) {return numInnerSectorGatingGridWires(i);}
  Int_t   	numberOfOuterSectorAnodeWires(Int_t i = 0)      {return numOuterSectorAnodeWires(i);}	    
  Int_t   	numberOfOuterSectorFrischGridWires(Int_t i = 0) {return numOuterSectorFrischGridWires(i);}
  Int_t   	numberOfOuterSectorGatingGridWires(Int_t i = 0) {return numOuterSectorGatingGridWires(i);}

 protected:
  St_tpcWirePlanesC(St_tpcWirePlanes *table=0) : TChair(table) {}
  virtual ~St_tpcWirePlanesC() {fgInstance = 0;}
 private:
  static St_tpcWirePlanesC* fgInstance;
  ClassDefChair(St_tpcWirePlanes, tpcWirePlanes_st )
  ClassDef(St_tpcWirePlanesC,1) //C++ TChair for tpcWirePlanes table class
};
#endif
