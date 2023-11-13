#ifndef St_spline3C_h
#define St_spline3C_h

#include "TChair.h"
//#include "tables/St_spline3_Table.h"
#include "St_spline3_Table.h"
#include "TSpline.h"
#include "TF1.h"
//________________________________________________________________________________
class St_spline3C : public TChair {
 public:
  St_spline3C(St_spline3 *table=0);
  virtual ~St_spline3C() {}
  spline3_st   *Struct(Int_t i = 0) 	const {return ((St_spline3*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	nknots(Int_t i = 0) 	const {return Struct(i)->nknots;}
  Double_t* 	Xknots(Int_t i = 0) 	const {return Struct(i)->Xknots;}
  Double_t* 	Yknots(Int_t i = 0) 	const {return Struct(i)->Yknots;}
  Double_t      ValBeg(Int_t i = 0) 	const {return Struct(i)->ValBeg;}
  Double_t      ValEnd(Int_t i = 0) 	const {return Struct(i)->ValEnd;}
  Char_t* 	option(Int_t i = 0) 	const {return (Char_t *) Struct(i)->option;}
  TSpline3*     Spline() {return fSpline;}
  Double_t      operator() (Double_t *x, Double_t *p) const {return fSpline->Eval(x[0]);}
  TF1*          Func() {return fFunc;}
  Bool_t        IsValid() {return fValid;}
  static        St_spline3  *Open(const Char_t *path);
  Bool_t        InRange(Double_t x) {return fXmin <= x && x <= fXmax;}
 private:
  TSpline3*     fSpline;
  TF1*          fFunc;
  Bool_t        fValid;
  Double_t      fXmin;
  Double_t      fXmax;
  ClassDefChair(St_spline3, spline3_st )
  ClassDef(St_spline3C,1) //C++ TChair for spline3 table class
};
//________________________________________________________________________________
class Stspline3LndNdxL10 : public St_spline3C {// Log(dN/dx) versus log10(beta*gamma)
 public:
  static Stspline3LndNdxL10* 	instance();
  Stspline3LndNdxL10(St_spline3 *table=0) : St_spline3C(table) {}
  virtual ~Stspline3LndNdxL10() {fgInstance = 0;}
 private:
  static Stspline3LndNdxL10* fgInstance;
  ClassDef(Stspline3LndNdxL10,1) //C++ TChair for spline3LndNdxL10
};
//________________________________________________________________________________
class StElectonsDEV_dEdx : public St_spline3C {// Log(dN/dx) versus log10(beta*gamma)
 public:
  static StElectonsDEV_dEdx* 	instance();
  StElectonsDEV_dEdx(St_spline3 *table=0) : St_spline3C(table) {}
  virtual ~StElectonsDEV_dEdx() {fgInstance = 0;}
 private:
  static StElectonsDEV_dEdx* fgInstance;
  ClassDef(StElectonsDEV_dEdx,1) //C++ TChair for ElectonsDEV_dEdx
};
//________________________________________________________________________________
class StPionDEV_dEdx : public St_spline3C {// Log(dN/dx) versus log10(beta*gamma)
 public:
  static StPionDEV_dEdx* 	instance();
  StPionDEV_dEdx(St_spline3 *table=0) : St_spline3C(table) {}
  virtual ~StPionDEV_dEdx() {fgInstance = 0;}
 private:
  static StPionDEV_dEdx* fgInstance;
  ClassDef(StPionDEV_dEdx,1) //C++ TChair for PionDEV_dEdx
};
//________________________________________________________________________________
class StProtonDEV_dEdx : public St_spline3C {// Log(dN/dx) versus log10(beta*gamma)
 public:
  static StProtonDEV_dEdx* 	instance();
  StProtonDEV_dEdx(St_spline3 *table=0) : St_spline3C(table) {}
  virtual ~StProtonDEV_dEdx() {fgInstance = 0;}
 private:
  static StProtonDEV_dEdx* fgInstance;
  ClassDef(StProtonDEV_dEdx,1) //C++ TChair for ProtonDEV_dEdx
};
#endif
