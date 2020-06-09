TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection3")) return 0;
  Int_t nrows = 1;
  MDFCorrection3_st row;
  St_MDFCorrection3 *tableSet = new St_MDFCorrection3("TpcAdcCorrection3MDF",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  for (Int_t i = 0; i < nrows; i++) {
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
