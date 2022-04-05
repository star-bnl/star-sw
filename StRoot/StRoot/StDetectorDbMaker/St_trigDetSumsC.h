#ifndef St_trigDetSumsC_h
#define St_trigDetSumsC_h

#include "TChair.h"
#include <math.h>
#include "tables/St_trigDetSums_Table.h"
#include "StDetectorDbClock.h"
#include "St_richvoltagesC.h"
#include "TMath.h"
class St_trigDetSumsC : public TChair {
 public:
  St_trigDetSumsC(St_trigDetSums *table=0) : TChair(table) {SafeDelete(fgInstance); fgInstance = this; fMargin = 0;}
  virtual ~St_trigDetSumsC() {fgInstance = 0;}
  static St_trigDetSumsC* 	instance();
  static St_trigDetSumsC*      GetInstance() {return fgInstance;}
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
  Double_t 	zdcCoin(Int_t i = 0)            {return Nc(zdcX(i),zdcEast(i),zdcWest(i));}
  Double_t 	bbcCoin(Int_t i = 0)            {return Nc(bbcX(i),bbcEast(i),bbcWest(i));}
  static St_trigDetSums* fgTableCopy;
  void		validityMargin(Double_t margin=0) {fMargin = margin;}
  Double_t getCTBWest() {return ctbWest();}
  Double_t getCTBEast() {return ctbEast();}
  Double_t getCTBOrTOFp() {return ctbTOFp();}
  Double_t getTOFp() {return tofp();}
  Double_t getZDCWest() {return zdcWest();}
  Double_t getZDCEast() {return zdcEast();}
  Double_t getZDCX() {return zdcX();}
  Double_t getZDCCoin() {return zdcCoin();}
  Double_t getMult() {return mult();}
  Double_t getL0() {return L0();}
  Double_t getBBCX() {return bbcX();}
  Double_t getBBCCoin() {return bbcCoin();}
  Double_t getBBCXCTB() {return bbcXctbTOFp();}
  Double_t getBBCWest() {return bbcWest();}
  Double_t getBBCEast() {return bbcEast();}
  Double_t getBBCYellowBkg() {return bbcYellowBkg();}
  Double_t getBBCBlueBkg() {return bbcBlueBkg();}
  Double_t getPVPDWest() {return pvpdWest();}
  Double_t getPVPDEast() {return pvpdEast();}
  UInt_t   getRichHVStatus() {return St_richvoltagesC::instance()->status();}
  void     setValidityMargin(Double_t margin=0) {validityMargin(margin);}

  // The following code attempts to correct coincidence rates for accidentals and multiples
  // See STAR Note 528
  static Double_t Nc(Double_t New, Double_t Ne, Double_t Nw, Int_t n_bunches=111) {
    // 111 is a guess using the maximum seen filled bunches in RHIC so far
    // (not always the case, but we don't have access to this number)
    Double_t Nbc = StDetectorDbClock::instance()->CurrentFrequency() * ((Double_t) n_bunches) / 120.;
    return -Nbc * TMath::Log(1. - ((New - (Ne*Nw/Nbc)) / (Nbc+New-Ne-Nw)));
  }

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
  ClassDef(St_trigDetSumsC,3) //C++ TChair for trigDetSums table class
};
#endif
