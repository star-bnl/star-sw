TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 192;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrectionX",nrows);
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  for (Int_t i = 0; i < nrows; i++) {
    tableSet->AddAt(&row); 
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
