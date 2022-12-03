TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.30517;
  row.a[1] =    0.0088408;
  row.a[2] =  -9.6645e-05;
  row.a[3] =   4.4887e-07;
  row.a[4] =  -7.2154e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.05898;
  row.a[1] =    0.0010689;
  row.a[2] =    3.435e-06;
  row.a[3] =  -1.0384e-07;
  row.a[4] =   3.6388e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.03172;
  row.a[1] =   0.00075545;
  row.a[2] =  -7.7477e-06;
  row.a[3] =   1.9975e-08;
  row.a[4] =   3.6248e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =      0.31786;
  row.a[1] =   -0.0097235;
  row.a[2] =   0.00010399;
  row.a[3] =  -5.0903e-07;
  row.a[4] =   9.8915e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
