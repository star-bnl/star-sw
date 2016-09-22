TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrectionX")) return 0;
  Int_t nrows = 2;
  tpcCorrectionX_st row;
  St_tpcCorrectionX *tableSet = new St_tpcCorrectionX("TpcCurrentCorrectionX",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

