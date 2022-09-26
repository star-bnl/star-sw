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
  row.npar =            7;// 5p75GeV_fixedTarget_2020
  row.a[0] =    0.0019954;
  row.a[1] =    0.0021388;
  row.a[2] =  -6.2605e-05;
  row.a[3] =   7.5876e-07;
  row.a[4] =  -4.8387e-09;
  row.a[5] =   1.5558e-11;
  row.a[6] =  -2.0272e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 5p75GeV_fixedTarget_2020
  row.a[0] =     0.090285;
  row.a[1] =    0.0025562;
  row.a[2] =  -0.00010444;
  row.a[3] =   1.2515e-06;
  row.a[4] =   -6.505e-09;
  row.a[5] =    1.202e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 5p75GeV_fixedTarget_2020
  row.a[0] =     -0.10913;
  row.a[1] =    0.0056146;
  row.a[2] =  -0.00014394;
  row.a[3] =   1.8721e-06;
  row.a[4] =  -1.2594e-08;
  row.a[5] =   4.2001e-11;
  row.a[6] =  -5.5082e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 5p75GeV_fixedTarget_2020
  row.a[0] =    -0.041035;
  row.a[1] =    0.0061569;
  row.a[2] =  -0.00014313;
  row.a[3] =   1.7059e-06;
  row.a[4] =  -1.2382e-08;
  row.a[5] =   4.8808e-11;
  row.a[6] =  -7.7167e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
