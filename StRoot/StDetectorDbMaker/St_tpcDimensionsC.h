#ifndef St_tpcDimensionsC_h
#define St_tpcDimensionsC_h

#include "TChair.h"
#include "tables/St_tpcDimensions_Table.h"
#include "St_tpcPadConfigC.h"
#include "St_tpcEffectiveGeomC.h"
#include "St_tpcWirePlanesC.h"
class St_tpcDimensionsC : public TChair {
 public:
  static St_tpcDimensionsC* 	instance();
  tpcDimensions_st 	*Struct(Int_t i = 0) 	{return ((St_tpcDimensions*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	numberOfSectors(Int_t i = 0) 	{return Struct(i)->numberOfSectors;}
  Double_t 	tpcInnerRadius(Int_t i = 0) 	{return Struct(i)->tpcInnerRadius;}
  Double_t 	tpcOuterRadius(Int_t i = 0) 	{return Struct(i)->tpcOuterRadius;}
  Double_t 	tpcTotalLength(Int_t i = 0) 	{return Struct(i)->tpcTotalLength;}
  Double_t 	wheelInnerRadius(Int_t i = 0) 	{return Struct(i)->wheelInnerRadius;}
  Double_t 	wheelOuterRadius(Int_t i = 0) 	{return Struct(i)->wheelOuterRadius;}
  Double_t 	wheelThickness(Int_t i = 0) 	{return Struct(i)->wheelThickness;}
  Double_t 	senseGasOuterRadius(Int_t i = 0){return Struct(i)->senseGasOuterRadius;}
  Double_t 	tpeaThickness(Int_t i = 0) 	{return Struct(i)->tpeaThickness;}
  Double_t 	cathodeInnerRadius(Int_t i = 0) {return Struct(i)->cathodeInnerRadius;}
  Double_t 	cathodeOuterRadius(Int_t i = 0) {return Struct(i)->cathodeOuterRadius;}
  Double_t 	cathodeThickness(Int_t i = 0) 	{return Struct(i)->cathodeThickness;}
  Double_t 	outerCuThickness(Int_t i = 0) 	{return Struct(i)->outerCuThickness;}
  Double_t 	outerKaptonThickness(Int_t i =0){return Struct(i)->outerKaptonThickness;}
  Double_t 	outerNomexThickness(Int_t i = 0){return Struct(i)->outerNomexThickness;}
  Double_t 	outerGlueThickness(Int_t i = 0) {return Struct(i)->outerGlueThickness;}
  Double_t 	outerInsGasThickness(Int_t i =0){return Struct(i)->outerInsGasThickness;}
  Double_t 	outerAlThickness(Int_t i = 0) 	{return Struct(i)->outerAlThickness;}
  Double_t 	outerAlHoneycombThickness(Int_t i=0) 	{return Struct(i)->outerAlHoneycombThickness;}
  Double_t 	innerGlueThickness(Int_t i = 0) {return Struct(i)->innerGlueThickness;}
  Double_t 	innerNomexThickness(Int_t i = 0){return Struct(i)->innerNomexThickness;}
  Double_t 	innerKaptonThickness(Int_t i =0){return Struct(i)->innerKaptonThickness;}
  Double_t 	innerAlThickness(Int_t i = 0) 	{return Struct(i)->innerAlThickness;}
  Double_t 	innerGapWidI(Int_t i = 0) 	{return Struct(i)->innerGapWidI;}
  Double_t 	innerGapWidO(Int_t i = 0) 	{return Struct(i)->innerGapWidO;}
  Double_t 	innerGapHeit(Int_t i = 0) 	{return Struct(i)->innerGapHeit;}
  Double_t 	innerGapRad(Int_t i = 0) 	{return Struct(i)->innerGapRad;}
  Double_t 	innerInWidth(Int_t i = 0) 	{return Struct(i)->innerInWidth;}
  Double_t 	innerOutWidth(Int_t i = 0) 	{return Struct(i)->innerOutWidth;}
  Double_t 	innerHeight(Int_t i = 0) 	{return Struct(i)->innerHeight;}
  Double_t 	innerPPDepth(Int_t i = 0) 	{return Struct(i)->innerPPDepth;}
  Double_t 	innerAlDepth(Int_t i = 0) 	{return Struct(i)->innerAlDepth;}
  Double_t 	innerMWCDepth(Int_t i = 0) 	{return Struct(i)->innerMWCDepth;}
  Double_t 	innerBoundary(Int_t i = 0) 	{return Struct(i)->innerBoundary;}
  Double_t 	innerRCenter(Int_t i = 0) 	{return Struct(i)->innerRCenter;}
  Double_t 	innerMWCInn(Int_t i = 0) 	{return Struct(i)->innerMWCInn;}
  Double_t 	innerMWCOut(Int_t i = 0) 	{return Struct(i)->innerMWCOut;}
  Double_t 	innerMVCHei(Int_t i = 0) 	{return Struct(i)->innerMVCHei;}
  Int_t 	innerAirGaps(Int_t i = 0) 	{return Struct(i)->innerAirGaps;}
  Int_t 	innerExtraAl(Int_t i = 0) 	{return Struct(i)->innerExtraAl;}
  Double_t* 	innerZGaps(Int_t i = 0) 	{return Struct(i)->innerZGaps;}
  Double_t* 	innerZGapsSize(Int_t i = 0) 	{return Struct(i)->innerZGapsSize;}
  Double_t* 	innerXExtraAl(Int_t i = 0) 	{return Struct(i)->innerXExtraAl;}
  Double_t* 	innerZExtraAl(Int_t i = 0) 	{return Struct(i)->innerZExtraAl;}
  Double_t* 	innerDXExtraAl(Int_t i = 0) 	{return Struct(i)->innerDXExtraAl;}
  Double_t* 	innerDZExtraAl(Int_t i = 0) 	{return Struct(i)->innerDZExtraAl;}
  Double_t 	outerGapWidI(Int_t i = 0) 	{return Struct(i)->outerGapWidI;}
  Double_t 	outerGapWidO(Int_t i = 0) 	{return Struct(i)->outerGapWidO;}
  Double_t 	outerGapHeit(Int_t i = 0) 	{return Struct(i)->outerGapHeit;}
  Double_t 	outerGapRad(Int_t i = 0) 	{return Struct(i)->outerGapRad;}
  Double_t 	outerInWidth(Int_t i = 0) 	{return Struct(i)->outerInWidth;}
  Double_t 	outerOutWidth(Int_t i = 0) 	{return Struct(i)->outerOutWidth;}
  Double_t 	outerHeight(Int_t i = 0) 	{return Struct(i)->outerHeight;}
  Double_t 	outerPPDepth(Int_t i = 0) 	{return Struct(i)->outerPPDepth;}
  Double_t 	outerAlDepth(Int_t i = 0) 	{return Struct(i)->outerAlDepth;}
  Double_t 	outerMWCDepth(Int_t i = 0) 	{return Struct(i)->outerMWCDepth;}
  Double_t 	outerBoundary(Int_t i = 0) 	{return Struct(i)->outerBoundary;}
  Double_t 	outerRCenter(Int_t i = 0) 	{return Struct(i)->outerRCenter;}
  Double_t 	outerMWCInn(Int_t i = 0) 	{return Struct(i)->outerMWCInn;}
  Double_t 	outerMWCOut(Int_t i = 0) 	{return Struct(i)->outerMWCOut;}
  Double_t 	outerMVCHei(Int_t i = 0) 	{return Struct(i)->outerMVCHei;}
  Int_t 	outerAirGaps(Int_t i = 0) 	{return Struct(i)->outerAirGaps;}
  Int_t 	outerExtraAl(Int_t i = 0) 	{return Struct(i)->outerExtraAl;}
  Double_t* 	outerZGaps(Int_t i = 0) 	{return Struct(i)->outerZGaps;}
  Double_t* 	outerZGapsSize(Int_t i = 0) 	{return Struct(i)->outerZGapsSize;}
  Double_t* 	outerXExtraAl(Int_t i = 0) 	{return Struct(i)->outerXExtraAl;}
  Double_t* 	outerZExtraAl(Int_t i = 0) 	{return Struct(i)->outerZExtraAl;}
  Double_t* 	outerDXExtraAl(Int_t i = 0) 	{return Struct(i)->outerDXExtraAl;}
  Double_t* 	outerDZExtraAl(Int_t i = 0) 	{return Struct(i)->outerDZExtraAl;}
  Double_t      gatingGridZ(Int_t sector=20) {
    return St_tpcPadConfigC::instance()->outerSectorPadPlaneZ(sector) 
      - St_tpcWirePlanesC::instance()->outerSectorGatingGridPadPlaneSeparation();}
  Double_t      zInnerOffset()                  {return St_tpcEffectiveGeomC::instance()->z_inner_offset();}
  Double_t      zOuterOffset()                  {return St_tpcEffectiveGeomC::instance()->z_outer_offset();}
  Double_t      zInnerOffset_West()             {return St_tpcEffectiveGeomC::instance()->z_inner_offset_West();}
  Double_t      zOuterOffset_West()             {return St_tpcEffectiveGeomC::instance()->z_outer_offset_West();}
  //TPC field cage parameters:
  Double_t ifcRadius() {return tpcInnerRadius();}
  Double_t ofcRadius() {return tpcOuterRadius();}

 protected:
  St_tpcDimensionsC(St_tpcDimensions *table=0) : TChair(table) {}
  virtual ~St_tpcDimensionsC() {fgInstance = 0;}
 private:
  static St_tpcDimensionsC* fgInstance;
  ClassDefChair(St_tpcDimensions, tpcDimensions_st )
  ClassDef(St_tpcDimensionsC,1) //C++ TChair for tpcDimensions table class
};
#endif
