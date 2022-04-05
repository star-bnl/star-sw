#ifndef St_VertexCutsC_h
#define St_VertexCutsC_h

#include "TChair.h"
#include "tables/St_VertexCuts_Table.h"

class St_VertexCutsC : public TChair {
 public:
  static St_VertexCutsC* 	instance();
  VertexCuts_st 	*Struct(Int_t i = 0) 	{return ((St_VertexCuts*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	MinNumberOfFitPointsOnTrack(Int_t i = 0) 	{return Struct(i)->MinNumberOfFitPointsOnTrack;}
  Int_t 	MinTrack(Int_t i = 0) 	{return Struct(i)->MinTrack;}
  Float_t 	DcaZMax(Int_t i = 0) 	{return Struct(i)->DcaZMax;}
  Double_t 	RImpactMax(Int_t i = 0) 	{return Struct(i)->RImpactMax;}
  Float_t	MinFracOfPossFitPointsOnTrack(Int_t i = 0)	{return Struct(i)->MinFracOfPossFitPointsOnTrack;}
  Float_t 	MinTrackPt(Int_t i = 0) 	{return Struct(i)->MinTrackPt;}
  Float_t 	ZMin(Int_t i = 0) 	{return Struct(i)->ZMin;}
  Float_t 	ZMax(Int_t i = 0) 	{return Struct(i)->ZMax;}
 protected:
  St_VertexCutsC(St_VertexCuts *table=0) : TChair(table) {}
  virtual ~St_VertexCutsC() {fgInstance = 0;}
 private:
  static St_VertexCutsC* fgInstance;
  ClassDefChair(St_VertexCuts, VertexCuts_st )
  ClassDef(St_VertexCutsC,1) //C++ TChair for VertexCuts table class
};
#endif
