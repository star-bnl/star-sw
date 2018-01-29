TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

