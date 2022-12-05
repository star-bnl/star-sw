TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.2;
  row.max =   1.2;
  row.npar =            4;// ps_OO_200GeV_2021
  row.a[0] =    -0.030905;
  row.a[1] =    -0.018508;
  row.a[2] =      0.32632;
  row.a[3] =      -0.2589;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -0.3;
  row.max =   1.8;
  row.npar =            7;// ps_OO_200GeV_2021
  row.a[0] =    -0.015045;
  row.a[1] =      0.04809;
  row.a[2] =     -0.22552;
  row.a[3] =     -0.88769;
  row.a[4] =       2.8332;
  row.a[5] =      -2.3028;
  row.a[6] =      0.57982;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =   0.2;
  row.npar =            4;// ps_OO_200GeV_2021
  row.a[0] =    -0.018455;
  row.a[1] =    0.0076641;
  row.a[2] =      0.19219;
  row.a[3] =      0.13689;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =   0.3;
  row.npar =            7;// ps_OO_200GeV_2021
  row.a[0] =    -0.021781;
  row.a[1] =    -0.063298;
  row.a[2] =      -0.3651;
  row.a[3] =      0.59086;
  row.a[4] =       2.6276;
  row.a[5] =       2.2582;
  row.a[6] =      0.58147;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
