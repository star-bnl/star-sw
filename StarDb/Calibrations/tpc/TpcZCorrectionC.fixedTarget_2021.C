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
  row.npar =            5;// fixetTarget_2021
  row.a[0] =    -0.027837;
  row.a[1] =  -0.00057298;
  row.a[2] =   1.8366e-05;
  row.a[3] =  -1.3961e-07;
  row.a[4] =   3.4262e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2021
  row.a[0] =    -0.022897;
  row.a[1] =   0.00056069;
  row.a[2] =   4.9776e-06;
  row.a[3] =  -9.4903e-08;
  row.a[4] =   3.0544e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2021
  row.a[0] =    -0.019492;
  row.a[1] =   0.00073304;
  row.a[2] =  -1.0407e-05;
  row.a[3] =   5.9903e-08;
  row.a[4] =  -1.1613e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2021
  row.a[0] =     -0.16042;
  row.a[1] =    0.0076195;
  row.a[2] =  -0.00010429;
  row.a[3] =   5.4822e-07;
  row.a[4] =  -9.8822e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
