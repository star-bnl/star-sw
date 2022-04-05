#ifndef StiLocalTrackSeedFinderParameters_h
#define StiLocalTrackSeedFinderParameters_h

#include "TChair.h"
#include "tables/St_LocalTrackSeedFinder_Table.h"

class StiLocalTrackSeedFinderParameters : public TChair {
 public:
  static StiLocalTrackSeedFinderParameters* 	instance();
  LocalTrackSeedFinder_st 	*Struct(Int_t i = 0) 	{return ((St_LocalTrackSeedFinder*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Double_t 	deltaY(Int_t i = 0) 	{return Struct(i)->deltaY;}
  Double_t 	deltaZ(Int_t i = 0) 	{return Struct(i)->deltaZ;}
  Double_t 	mExtrapDeltaY(Int_t i = 0) 	{return Struct(i)->mExtrapDeltaY;}
  Double_t 	mExtrapDeltaZ(Int_t i = 0) 	{return Struct(i)->mExtrapDeltaZ;}
  Int_t 	seedLength(Int_t i = 0) 	{return Struct(i)->seedLength;}
  Int_t 	maxSkipped(Int_t i = 0) 	{return Struct(i)->maxSkipped;}
  Int_t 	extrapMaxLength(Int_t i = 0) 	{return Struct(i)->extrapMaxLength;}
  Int_t 	extrapMinLength(Int_t i = 0) 	{return Struct(i)->extrapMinLength;}
  Int_t 	useOrigin(Int_t i = 0) 	{return Struct(i)->useOrigin;}
  Double_t 	extrapDeltaY() 	{return mExtrapDeltaY();}
  Double_t 	extrapDeltaZ() 	{return mExtrapDeltaZ();}
  
 protected:
  StiLocalTrackSeedFinderParameters(St_LocalTrackSeedFinder *table=0) : TChair(table) {}
  virtual ~StiLocalTrackSeedFinderParameters() {fgInstance = 0;}
 private:
  static StiLocalTrackSeedFinderParameters* fgInstance;
  ClassDefineChair(StiLocalTrackSeedFinderParameters,St_LocalTrackSeedFinder, LocalTrackSeedFinder_st )
  ClassDef(StiLocalTrackSeedFinderParameters,1) //C++ TChair for LocalTrackSeedFinder table class
};
#endif
