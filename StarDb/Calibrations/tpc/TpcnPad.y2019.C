TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 1;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcnPad",nrows);
  memset(&row,0,tableSet->GetRowSize());
   row.idx = 1;
  row.nrows = nrows;
  row.min  = 2.5;
  row.max = 16.5;
 tableSet->AddAt(&row);// Outer & Inner 
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
