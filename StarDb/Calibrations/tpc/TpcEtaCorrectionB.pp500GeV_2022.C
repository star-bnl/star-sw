TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   1.4;
  row.npar =            1;// pp500GeV_2022
  row.a[0] =    0.0096317;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.7;
  row.max =   1.7;
  row.npar =            1;// pp500GeV_2022
  row.a[0] =    0.0042193;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
