#ifndef StiHitErrorCalculator_h
#define StiHitErrorCalculator_h

#include "TChair.h"
#include "tables/St_HitError_Table.h"
#include "Sti/StiNodePars.h"

class StiHitErrorCalculator : public TChair {
 public:
  HitError_st 	*Struct(Int_t i = 0) 	{return ((St_HitError*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            {return GetNRows();}
  Double_t* 	coeff(Int_t i = 0) 	{return Struct(i)->coeff;} /// coeff[6] = 0:intrinsicY  1: driftY   2: crossY 3:intrinsicZ  4: driftZ   5: crossZ
  virtual void  calculateError(Double_t _z,  Double_t _eta, Double_t _tanl, Double_t &ecross, Double_t &edip) const;
  virtual void  calculateError(const StiNodePars *pars,Double_t &ecross,Double_t &edip) const {
    calculateError(pars->z(),  pars->eta(), pars->tanl(), ecross, edip);
  }
 protected:
  StiHitErrorCalculator(St_HitError *table=0) : TChair(table) {}
  virtual ~StiHitErrorCalculator() {}
 private:
  ClassDefineChair(StiHitErrorCalculator,St_HitError, HitError_st )
  ClassDef(StiHitErrorCalculator,1) //C++ TChair for HitError table class
};
#endif
