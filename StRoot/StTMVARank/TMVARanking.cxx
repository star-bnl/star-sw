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
#include "PVgadgets.h"
TableImpl(PVgadgets);
TMVAdata* TMVAdata::fgInstance = 0;
//________________________________________________________________________________
TMVA::Reader *TMVARanking(const Char_t *listOfActiveVariable, const Char_t *weightfile) {
  // This loads the library
  TMVA::Tools::Instance();
  const Char_t *method = "BDT";
  TMVA::Reader *reader = new TMVA::Reader( "!Color:!Silent" );   
  Bool_t iPPV = kFALSE;
  //  TMVAdata::instance()->fAcceptMap["postx"] = kFALSE;
  Float_t *dataArray = (Float_t *) TMVAdata::instance()->GetArray();
  TTableDescriptor *ds = TMVAdata::instance()->GetTableDesc();
  tableDescriptor_st *s = ds->GetTable();
  for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
    TString aName(s->fColumnName);
    if (! TMVAdata::instance()->AcceptVar(aName)) {
      reader->AddSpectator(aName,  dataArray+i);
    } else {
      reader->AddVariable(aName, dataArray+i);
    }
    i++;
  }
#if 0
  TString dir    = "weights/";
  TString prefix = "TMVAClassification";
  TString weightfile = dir + prefix + TString("_") + method + TString(".weights.xml");
#endif
  reader->BookMVA(method, weightfile ); 
  return reader;
}
