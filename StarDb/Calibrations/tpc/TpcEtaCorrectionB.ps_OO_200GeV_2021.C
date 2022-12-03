TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =   1.2;
  row.npar =            1;// ps_OO_200GeV_2021
  row.a[0] =    0.0093412;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =   1.8;
  row.npar =            1;// ps_OO_200GeV_2021
  row.a[0] =   -0.0082838;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
