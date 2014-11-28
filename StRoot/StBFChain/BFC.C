#if !defined(__CINT__)
#include "TROOT.h"
#endif
#include "Bfc.h"
#if !defined(__CINT__)
TableImpl(Bfc);
#endif
#include "BigFullChain.h"
TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Bfc")) return 0;
  Int_t NoChainOptions = sizeof (BFC)/sizeof (Bfc_st);
  St_Bfc *tableSet = new St_Bfc("BFC",NoChainOptions); 
  for (Int_t i = 0; i < NoChainOptions; i++) {
    tableSet->AddAt(&BFC[i]);
  }
  return tableSet;
}
