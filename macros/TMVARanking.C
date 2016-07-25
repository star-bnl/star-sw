/**********************************************************************************
 * Project   : TMVA - a Root-integrated toolkit for multivariate data analysis    *
 * Package   : TMVA                                                               *
 * Exectuable: TMVAClassificationApplication                                      *
 *                                                                                *
 * This macro provides a simple example on how to use the trained classifiers     *
 * within an analysis module                                                      *
 **********************************************************************************/
#include "TMVA/Tools.h"
#include "TMVA/Reader.h"
#include "TMVA/MethodCuts.h"
#include "TMVAdata.h"
#include "TString.h"
#include <map>
using namespace TMVA;
//________________________________________________________________________________
TMVA::Reader *TMVARanking() {
  // This loads the library
  TMVA::Tools::Instance();
  const Char_t *method = "BDT";
  TMVA::Reader *reader = new TMVA::Reader( "!Color:!Silent" );   
  Bool_t iPPV = kFALSE;
  TMVAdata data;
  data.Init(iPPV);
  data.AcceptMap["postx"] = kFALSE;
  TString separator(":");
  TString Vnames(TMVAdata::vnames);
  TObjArray *array = Vnames.Tokenize(separator);
  
  TIter next(array);
  TObjString *objs;
  Float_t *dataArray = &data.postx;
  Int_t i = 0;
  while ((objs = (TObjString *) next())) {
    //    cout << objs->GetString() << endl;
    if (! data.AcceptVar(iPPV, objs->GetString())) {
      reader->AddSpectator(objs->GetString(),  dataArray+i);
    } else {
      reader->AddVariable(objs->GetString(), dataArray+i);
    }
    i++;
  }
  TString dir    = "weights/";
  TString prefix = "TMVAClassification";
  TString weightfile = dir + prefix + TString("_") + method + TString(".weights.xml");
  reader->BookMVA(method, weightfile ); 
  return reader;
}
