TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// 11p5GeV_2020
  row.a[0] =       0.0703;
  row.a[1] =  -0.00034682;
  row.a[2] =  -3.6638e-06;
  row.a[3] =   2.3308e-08;
  row.a[4] =  -4.6713e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 11p5GeV_2020
  row.a[0] =     0.059965;
  row.a[1] =    0.0047818;
  row.a[2] =  -7.4754e-05;
  row.a[3] =   3.2751e-07;
  row.a[4] =  -4.4684e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
