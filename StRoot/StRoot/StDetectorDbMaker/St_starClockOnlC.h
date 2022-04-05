#ifndef St_starClockOnlC_h
#define St_starClockOnlC_h

#include "TChair.h"
#include "tables/St_starClockOnl_Table.h"

class St_starClockOnlC : public TChair {
 public:
  static St_starClockOnlC*   instance();
  starClockOnl_st 	*Struct(Int_t i = 0);
  UInt_t    getNumRows()                      {return GetNRows();}
  UInt_t    RunNumber(Int_t i = 0)           {return Struct(i)->runNumber;}
  Double_t  CurrentFrequency(Int_t i = 0)    {return Struct(i)->frequency;} 
  UInt_t    Time(Int_t i = 0)                {return Struct(i)->time;}   
  Double_t  Frequency(Int_t i = 0)           {return CurrentFrequency(i);}
  // depreciated
  UInt_t    getRunNumber(Int_t i = 0)        {return RunNumber(i);}       
  Double_t  getCurrentFrequency(Int_t i = 0) {return CurrentFrequency(i);} 
  UInt_t    getTime(Int_t i = 0)             {return Time(i);}            
  Double_t  getFrequency(Int_t i = 0)        {return Frequency(i);}       
  Double_t  samplingFrequency(Int_t i = 0)   {return 1e-6*CurrentFrequency(i);}
 protected:
  St_starClockOnlC(St_starClockOnl *table=0) : TChair(table) {}
  virtual ~St_starClockOnlC() {fgInstance = 0;}
 private:
  static St_starClockOnlC* fgInstance;
  ClassDefChair(St_starClockOnl, starClockOnl_st )
  ClassDef(St_starClockOnlC,1) //C++ TChair for starClockOnl table class
};
#endif
