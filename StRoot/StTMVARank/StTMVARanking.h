#ifndef __StTMVARanking_h__
#define __StTMVARanking_h__
#include "TMVA/Reader.h"
#include <map>
#include <string>
using namespace TMVA;
class StPrimaryVertex;
class StMuPrimaryVertex;
class StTMVARanking : public TObject {
 public:
  StTMVARanking(const Char_t *listOfActiveVariable = "", const Char_t *weightfile = "", const Char_t *Method = "BDT");
  virtual        ~StTMVARanking()  {delete fgReader;}
  static StTMVARanking  *instance() {return fgInstance;}
  static TMVA::Reader   *Reader()   {return fgReader;}
  static TString        &Method()   {return *&fMethod;}
  static Double_t        Evaluate() {return fgReader->EvaluateMVA(fMethod);}
  static void            TMVAClassification( TString myMethodList, TTree *signal, TTree *background);
  static Float_t         TMVARank(StPrimaryVertex *primV);
  static Float_t         TMVARank(StMuPrimaryVertex *primV);
  static Float_t         SimpleMindedRank(StPrimaryVertex *primV);
  static Float_t         SimpleMindedRank(StMuPrimaryVertex *primV);
 private:
  static StTMVARanking* fgInstance;
  static TMVA::Reader  *fgReader;
  static TString        fMethod;
  static TString        fMethodE;
  ClassDef(StTMVARanking,1)
};
#endif /* __StTMVARanking_h__ */
