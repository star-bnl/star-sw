#define STR_OBSOLETE "WARNING *** Option is OBSOLETE ***"
#define __BFC2__
#include "BigFullChain.h"
TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Bfc")) return 0;
  Int_t NoChainOptions = sizeof (BFC2)/sizeof (Bfc_st);;
  St_Bfc *tableSet = new St_Bfc("BFC2",NoChainOptions);
  for (Int_t i = 0; i < NoChainOptions; i++) {
    tableSet->AddAt(&BFC2[i]);
  }
  return tableSet;
}
