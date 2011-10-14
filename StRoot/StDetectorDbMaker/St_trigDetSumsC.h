#ifndef St_trigDetSumsC_h
#define St_trigDetSumsC_h

#include "TChair.h"
#include <math.h>
#include "tables/St_trigDetSums_Table.h"

class St_trigDetSumsC : public TChair {
 public:
  St_trigDetSumsC(St_trigDetSums *table) : TChair(table) {SafeDelete(fgInstance); fgInstance = this; fMargin = 0;}
  virtual ~St_trigDetSumsC() {fgInstance = 0;}
  static St_trigDetSumsC* 	instance()      {return fgInstance;}
  trigDetSums_st 	*Struct(Int_t i = 0) 	{return ((St_trigDetSums*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  UInt_t 	timeOffset(Int_t i = 0) 	{return Struct(i)->timeOffset;}
  Double_t 	ctbWest(Int_t i = 0) 	        {return validity(Struct(i)->ctbWest);}
  Double_t 	ctbEast(Int_t i = 0) 	        {return validity(Struct(i)->ctbEast);}
  Double_t 	ctbTOFp(Int_t i = 0) 	        {return validity(Struct(i)->ctbTOFp);}
  Double_t 	tofp(Int_t i = 0) 	        {return validity(Struct(i)->tofp);}
  Double_t 	zdcWest(Int_t i = 0) 	        {return validity(Struct(i)->zdcWest);}
  Double_t 	zdcEast(Int_t i = 0) 	        {return validity(Struct(i)->zdcEast);}
  Double_t 	zdcX(Int_t i = 0) 	        {return validity(Struct(i)->zdcX);}
  Double_t 	mult(Int_t i = 0) 	        {return validity(Struct(i)->mult);}
  Double_t 	L0(Int_t i = 0) 	        {return validity(Struct(i)->L0);}
  Double_t 	bbcX(Int_t i = 0) 	        {return validity(Struct(i)->bbcX);}
  Double_t 	bbcXctbTOFp(Int_t i = 0) 	{return validity(Struct(i)->bbcXctbTOFp);}
  Double_t 	bbcWest(Int_t i = 0) 	        {return validity(Struct(i)->bbcWest);}
  Double_t 	bbcEast(Int_t i = 0) 	        {return validity(Struct(i)->bbcEast);}
  Double_t 	bbcYellowBkg(Int_t i = 0) 	{return validity(Struct(i)->bbcYellowBkg);}
  Double_t 	bbcBlueBkg(Int_t i = 0) 	{return validity(Struct(i)->bbcBlueBkg);}
  Double_t 	pvpdWest(Int_t i = 0) 	        {return validity(Struct(i)->pvpdWest);}
  Double_t 	pvpdEast(Int_t i = 0) 	        {return validity(Struct(i)->pvpdEast);}
  static St_trigDetSums* fgTableCopy;
  void		validityMargin(Double_t margin=0) {fMargin = margin;}
 protected:
 private:
  static St_trigDetSumsC* fgInstance;
  Double_t	fMargin;

  // The following code is meant to handle "stuck" RICH Scalers, where the scaler
  // integrates rates for a factor of 2 or 3 times as long as it is expected to,
  // leading to erroneous numbers.
  // validity margin parameter is passed (probably from spaceChargeCor)
  Double_t validity(double& newval) {

    if (!fgTableCopy)
      fgTableCopy = new St_trigDetSums(*((St_trigDetSums*) Table()));

    // reference pointer to previous table's data
    double& oldval = *((double*) (((char*) (fgTableCopy->GetTable())) +
                      (((char*) &newval) - ((char*) Struct(0)))));

    // Corrupt scalers >1e9 seen in Run 9 (not possible at RHIC)
    // Will return previous value, unless it is also corrupt,
    // otherwise an unphysical value (-1) which can be used to trap problems
    double CORRUPT = 1e9;
    if (newval > CORRUPT || newval < 0)
      return (oldval > CORRUPT  || oldval < 0 ? -1 : oldval);

    if (fMargin == 0) return newval;

    // Use newval for low rates, below (x2-margin), outside (multiple+/-margin)
    if ((oldval < 100) ||
        ((newval/oldval) < (2.0-fMargin)) ||
        (fabs(fmod((newval/oldval)+0.5,1.0)-0.5) > fMargin))
      oldval = newval;

    return oldval;
  }

  ClassDefChair(St_trigDetSums, trigDetSums_st )
  ClassDef(St_trigDetSumsC,2) //C++ TChair for trigDetSums table class
};
#endif
