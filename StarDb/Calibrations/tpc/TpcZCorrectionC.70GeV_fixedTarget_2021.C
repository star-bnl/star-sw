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
  row.npar =            3;// 70GeV_fixedTarget_2021
  row.a[0] =    0.0027358;
  row.a[1] =   0.00049223;
  row.a[2] =  -3.6995e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 70GeV_fixedTarget_2021
  row.a[0] =     0.072606;
  row.a[1] =    0.0002761;
  row.a[2] =  -5.5932e-05;
  row.a[3] =   1.3303e-06;
  row.a[4] =  -1.3405e-08;
  row.a[5] =   5.8083e-11;
  row.a[6] =  -9.0994e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 70GeV_fixedTarget_2021
  row.a[0] =    -0.080479;
  row.a[1] =    0.0030971;
  row.a[2] =  -9.5425e-05;
  row.a[3] =   1.5568e-06;
  row.a[4] =  -1.2937e-08;
  row.a[5] =   5.2638e-11;
  row.a[6] =  -8.3184e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 70GeV_fixedTarget_2021
  row.a[0] =      0.29105;
  row.a[1] =   -0.0066318;
  row.a[2] =   1.8422e-05;
  row.a[3] =   1.0828e-06;
  row.a[4] =   -1.538e-08;
  row.a[5] =   7.8803e-11;
  row.a[6] =  -1.4066e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
