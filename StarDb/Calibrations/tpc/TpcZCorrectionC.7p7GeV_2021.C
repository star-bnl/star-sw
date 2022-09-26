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
  row.npar =            2;// 7p7GeV_2021
  row.a[0] =    0.0024919;
  row.a[1] =    1.149e-05;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2021
  row.a[0] =    -0.018956;
  row.a[1] =    0.0039494;
  row.a[2] =  -5.3397e-05;
  row.a[3] =   2.6059e-07;
  row.a[4] =  -4.3996e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            2;// 7p7GeV_2021
  row.a[0] =    0.0037624;
  row.a[1] =    2.556e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2021
  row.a[0] =    -0.015727;
  row.a[1] =    0.0047081;
  row.a[2] =  -6.4843e-05;
  row.a[3] =   3.1987e-07;
  row.a[4] =  -5.4209e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
