TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.6;
  row.max =   1.4;
  row.npar =            4;// pp500GeV_2022
  row.a[0] =    -0.036614;
  row.a[1] =     0.061736;
  row.a[2] =     0.098582;
  row.a[3] =     -0.10792;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -0.8;
  row.max =   1.7;
  row.npar =            9;// pp500GeV_2022
  row.a[0] =    0.0082683;
  row.a[1] =     0.028322;
  row.a[2] =     -0.22046;
  row.a[3] =     0.046635;
  row.a[4] =      0.75142;
  row.a[5] =     -0.50748;
  row.a[6] =     -0.63737;
  row.a[7] =      0.74131;
  row.a[8] =     -0.19727;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   0.7;
  row.npar =            4;// pp500GeV_2022
  row.a[0] =     -0.03109;
  row.a[1] =    -0.054724;
  row.a[2] =     0.094542;
  row.a[3] =      0.10084;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.7;
  row.max =   0.8;
  row.npar =            7;// pp500GeV_2022
  row.a[0] =   -0.0034115;
  row.a[1] =   -0.0066378;
  row.a[2] =    -0.068179;
  row.a[3] =     -0.15724;
  row.a[4] =     0.054503;
  row.a[5] =      0.21145;
  row.a[6] =      0.08382;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
