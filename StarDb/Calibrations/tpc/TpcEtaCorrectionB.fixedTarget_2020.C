TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =   0.0;
  row.npar =            7;// fixetTarget_2020
  row.a[0] =    -0.013258;
  row.a[1] =     -0.93353;
  row.a[2] =      -5.9305;
  row.a[3] =      -17.635;
  row.a[4] =       -26.76;
  row.a[5] =      -19.931;
  row.a[6] =      -5.8502;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   0.0;
  row.npar =            6;// fixetTarget_2020
  row.a[0] =      0.20484;
  row.a[1] =     -0.17843;
  row.a[2] =       -1.077;
  row.a[3] =      -1.5873;
  row.a[4] =      -1.0912;
  row.a[5] =     -0.26303;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.8;
  row.npar =            4;// fixetTarget_2020
  row.a[0] =      -3.1893;
  row.a[1] =      -5.9603;
  row.a[2] =       -3.586;
  row.a[3] =     -0.69363;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.2;
  row.max =  -1.2;
  row.npar =            3;// fixetTarget_2020
  row.a[0] =      -2.9277;
  row.a[1] =      -2.9749;
  row.a[2] =     -0.73833;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
