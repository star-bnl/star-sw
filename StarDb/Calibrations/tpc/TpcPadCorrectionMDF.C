#ifdef __CLING__
#  include "tables/St_MDFCorrection_Table.h"
#endif
#ifdef __CLING__
#  include "tables/St_MDFCorrection_Table.h"
#endif
TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  Int_t nrows = 48;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcPadCorrectionMDF",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  for (Int_t i = 0; i < 48; i++) {
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
