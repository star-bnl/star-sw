// $Id: StHFillVars.h,v 1.1 1999/08/31 20:40:20 genevb Exp $
// $Log: StHFillVars.h,v $
// Revision 1.1  1999/08/31 20:40:20  genevb
// Introduction of library and inclusion of StHFillObject
//
// Revision 1.1  1999/08/10 21:29:21  genevb
// Use formulas, separate headers, use StMessMgr, spaces no longer separate
//
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StHFillVars provides services for StHFillObject                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StHFillVars
#define STAR_StHFillVars

#include "TFormula.h"
class TMethodCall;
class TH1;
class TList;
class TObjArray;
class StHFillFormula;
class TObject;
class TArrayI;
class TArrayD;

static const size_t dimMax = 8;
static const size_t dimMax1 = dimMax-1;
static const size_t nsMax = 50;
static const size_t bufSize = 1024;

class StHFillVars {
 public:
  StHFillVars();
  ~StHFillVars();
  void ClearBuffer();
  void ClearSets();
  void ClearUsed();
  void Setup(Option_t* option, Int_t hists=1);
  void GetValues(Int_t printIt=0);
  
  Char_t buffer[bufSize];
  Char_t buffer1[bufSize];
  Float_t values[nsMax][dimMax];
  Stat_t weight[nsMax];
  TMethodCall* methods[nsMax][dimMax];
  StHFillFormula* formula[nsMax][dimMax];
  Char_t* opt[nsMax];
  size_t dims[nsMax];
  size_t dim;
  size_t nsets;
  size_t set;
  TH1* histo[nsMax];
  TClass* thisClass;
  TClass* oldClass;
  TList* pubMembers;
  TList* pubMethods;
  Int_t methMem;
  Int_t accSize;
  TObjArray* access;
  TArrayD* calcs;
  TArrayI* valPtrs;
  TArrayI* used[nsMax][dimMax];
  size_t nused[nsMax][dimMax];
  TObject* obj;
};

class StHFillFormula : public TFormula {
 public:
  StHFillFormula(const char* expression);
  virtual ~StHFillFormula();
  Int_t DefinedVariable(TString& variable);
  Double_t DefinedValue(Int_t code);
  Double_t Eval();
};

#endif
