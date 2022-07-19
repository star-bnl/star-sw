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
  row.npar =            7;// 19p5GeV_fixedTarget_2020
  row.a[0] =     0.042111;
  row.a[1] =    -0.001391;
  row.a[2] =   2.2534e-05;
  row.a[3] =  -2.4235e-07;
  row.a[4] =   2.0111e-09;
  row.a[5] =  -9.8628e-12;
  row.a[6] =    1.828e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 19p5GeV_fixedTarget_2020
  row.a[0] =      0.11428;
  row.a[1] =   -0.0015538;
  row.a[2] =   3.2525e-05;
  row.a[3] =  -3.8737e-07;
  row.a[4] =   1.7135e-09;
  row.a[5] =  -3.7885e-12;
  row.a[6] =   5.0558e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 19p5GeV_fixedTarget_2020
  row.a[0] =    -0.035644;
  row.a[1] =    0.0043915;
  row.a[2] =  -0.00012653;
  row.a[3] =   1.8353e-06;
  row.a[4] =  -1.4173e-08;
  row.a[5] =   5.4443e-11;
  row.a[6] =  -8.1741e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 19p5GeV_fixedTarget_2020
  row.a[0] =     -0.21639;
  row.a[1] =     0.024516;
  row.a[2] =  -0.00065554;
  row.a[3] =   8.7871e-06;
  row.a[4] =   -6.419e-08;
  row.a[5] =   2.3887e-10;
  row.a[6] =  -3.5222e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
