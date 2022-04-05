#ifndef St_starMagAvgC_h
#define St_starMagAvgC_h

#include "TChair.h"
#include "tables/St_starMagAvg_Table.h"

class St_starMagAvgC : public TChair {
 public:
  static St_starMagAvgC* 	instance();
  starMagAvg_st 	*Struct(Int_t i = 0) 	const {return ((St_starMagAvg*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	const {return Struct(i)->runNumber;}
  UInt_t 	startRunTime(Int_t i = 0) 	const {return Struct(i)->startRunTime;}
  UInt_t 	endRunTime(Int_t i = 0) 	const {return Struct(i)->endRunTime;}
  UInt_t 	noEntries(Int_t i = 0) 	const {return Struct(i)->noEntries;}
  Double_t 	current(Int_t i = 0) 	const {return Struct(i)->current;}
  Double_t 	rms(Int_t i = 0) 	const {return Struct(i)->rms;}
  Double_t      ScaleFactor(Int_t i = 0){return current()/(4500/(9.98071899596718826e-01));} // 12/03/20 scale Run 2020 11p5GeV K_s^0 (pcmax(0.49827)/pcmax(0.497611)) to PDG value 
  StMagnetPolarity           getMagneticField() {
    StMagnetPolarity value = eUnknownMField;
    if (! instance()) return value;
    Double_t scaleFactor = ScaleFactor();
    if      (scaleFactor >  0.75)	value = eFullMFieldPolA;
    else if (scaleFactor >  0.10)	value = eHalfMFieldPolA;
    else if (scaleFactor > -0.10)	value = eZeroMField;
    else if (scaleFactor > -0.75)	value = eHalfMFieldPolB;
    else if (scaleFactor > -1.25)	value = eFullMFieldPolB;
    return value;
  }
 protected:
  St_starMagAvgC(St_starMagAvg *table=0) : TChair(table) {}
  virtual ~St_starMagAvgC() {fgInstance = 0;}
 private:
  static St_starMagAvgC* fgInstance;
  ClassDefChair(St_starMagAvg, starMagAvg_st )
  ClassDef(St_starMagAvgC,1) //C++ TChair for starMagAvg table class
};
#endif
