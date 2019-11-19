#ifndef St_beamInfoC_h
#define St_beamInfoC_h

#include "TChair.h"
#include "tables/St_beamInfo_Table.h"

class St_beamInfoC : public TChair {
 public:
  static St_beamInfoC* 	instance();
  beamInfo_st 	*Struct(Int_t i = 0) 	          {return ((St_beamInfo*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	  {return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	          {return Struct(i)->runNumber;}
  Int_t 	entryTag(Int_t i = 0) 	          {return Struct(i)->entryTag;}
  Char_t* 	blueSpecies(Int_t i = 0) 	  {return Struct(i)->blueSpecies;}
  UInt_t 	blueMassNumber(Int_t i = 0) 	  {return Struct(i)->blueMassNumber;}
  Float_t 	blueEnergy(Int_t i = 0) 	  {return Struct(i)->blueEnergy;}
  Float_t 	blueIntensity(Int_t i = 0) 	  {return Struct(i)->blueIntensity;}
  Float_t 	blueLifeTime(Int_t i = 0) 	  {return Struct(i)->blueLifeTime;}
  Float_t 	blueBunchIntensity(Int_t i = 0)   {return Struct(i)->blueBunchIntensity;}
  Char_t* 	yellowSpecies(Int_t i = 0) 	  {return Struct(i)->yellowSpecies;}
  UInt_t 	yellowMassNumber(Int_t i = 0) 	  {return Struct(i)->yellowMassNumber;}
  Float_t 	yellowEnergy(Int_t i = 0) 	  {return Struct(i)->yellowEnergy;}
  Float_t 	yellowIntensity(Int_t i = 0) 	  {return Struct(i)->yellowIntensity;}
  Float_t 	yellowLifeTime(Int_t i = 0) 	  {return Struct(i)->yellowLifeTime;}
  Float_t 	yellowBunchIntensity(Int_t i =0)  {return Struct(i)->yellowBunchIntensity;}
  Float_t 	blueFillNumber(Int_t i = 0) 	  {return Struct(i)->blueFillNumber;}
  Float_t 	yellowFillNumber(Int_t i = 0) 	  {return Struct(i)->yellowFillNumber;}
  UInt_t        getRunNumber(Int_t i=0)           {return runNumber(i);}
  Int_t         getEntryTag(Int_t i=0)            {return entryTag(i);}
  Char_t*       getBlueSpecies(Int_t i=0)         {return blueSpecies(i);}
  UInt_t        getBlueMassNumber(Int_t i=0)      {return blueMassNumber(i);}
  Float_t       getBlueEnergy(Int_t i=0)          {return blueEnergy(i);}
  Float_t       getBlueIntensity(Int_t i=0)       {return blueIntensity(i);}
  Float_t       getBlueLifeTime(Int_t i=0)        {return blueLifeTime(i);}
  Float_t       getBlueBunchIntensity(Int_t i=0)  {return blueBunchIntensity(i);}
  Float_t       getBlueFillNumber(Int_t i=0)      {return blueFillNumber(i);}
  Char_t*       getYellowSpecies(Int_t i=0)       {return yellowSpecies(i);}
  UInt_t        getYellowMassNumber(Int_t i=0)    {return yellowMassNumber(i);}
  Float_t       getYellowEnergy(Int_t i=0)        {return yellowEnergy(i);}
  Float_t       getYellowIntensity(Int_t i=0)     {return yellowIntensity(i);}
  Float_t       getYellowLifeTime(Int_t i=0)      {return yellowLifeTime(i);}
  Float_t       getYellowBunchIntensity(Int_t i=0){return yellowBunchIntensity(i);}
  Float_t       getYellowFillNumber(Int_t i=0)    {return yellowFillNumber(i);}
  Bool_t        IsFixedTarget();
 protected:
  St_beamInfoC(St_beamInfo *table=0) : TChair(table) {}
  virtual ~St_beamInfoC() {fgInstance = 0;}
 private:
  static St_beamInfoC* fgInstance;
  ClassDefChair(St_beamInfo, beamInfo_st )
  ClassDef(St_beamInfoC,1) //C++ TChair for beamInfo table class
};
#endif
