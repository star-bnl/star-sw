TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 1;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcnTbk",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 1;
  row.nrows = nrows;
  row.type = 200;
  row.min  = 3.5;
  row.max = 16.5;
  tableSet->AddAt(&row);// Inner & Outer
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
