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
  row.a[0] =    0.0022689;
  row.a[1] =   0.00087477;
  row.a[2] =   -1.784e-05;
  row.a[3] =   8.1886e-08;
  row.a[4] =   4.5726e-10;
  row.a[5] =  -4.6898e-12;
  row.a[6] =   9.5718e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 5p75GeV_fixedTarget_2020
  row.a[0] =    -0.059553;
  row.a[1] =     0.012194;
  row.a[2] =  -0.00038893;
  row.a[3] =   5.5547e-06;
  row.a[4] =  -4.0848e-08;
  row.a[5] =   1.4779e-10;
  row.a[6] =  -2.0833e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 5p75GeV_fixedTarget_2020
  row.a[0] =    -0.013463;
  row.a[1] =    0.0026202;
  row.a[2] =  -6.9111e-05;
  row.a[3] =   9.0152e-07;
  row.a[4] =  -6.4411e-09;
  row.a[5] =   2.3318e-11;
  row.a[6] =  -3.3261e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 5p75GeV_fixedTarget_2020
  row.a[0] =     -0.10912;
  row.a[1] =     0.014763;
  row.a[2] =  -0.00038682;
  row.a[3] =   5.1882e-06;
  row.a[4] =  -3.9064e-08;
  row.a[5] =   1.5144e-10;
  row.a[6] =  -2.3234e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
