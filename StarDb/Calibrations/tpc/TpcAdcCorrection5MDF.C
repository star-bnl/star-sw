TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection4")) return 0;
  Int_t nrows = 2;
  MDFCorrection4_st row;
  St_MDFCorrection4 *tableSet = new St_MDFCorrection4("TpcAdcCorrection5MDF",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  for (Int_t i = 0; i < nrows; i++) {
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
