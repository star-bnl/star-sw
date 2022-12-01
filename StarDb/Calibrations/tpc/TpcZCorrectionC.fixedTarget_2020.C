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
  row.npar =            5;// fixetTarget_2020
  row.a[0] =   -0.0026061;
  row.a[1] =   -8.662e-05;
  row.a[2] =   1.8233e-06;
  row.a[3] =  -1.3022e-08;
  row.a[4] =   4.7811e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2020
  row.a[0] =     0.011724;
  row.a[1] =  -0.00063152;
  row.a[2] =   2.1906e-05;
  row.a[3] =  -1.9956e-07;
  row.a[4] =   5.4551e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2020
  row.a[0] =    -0.011248;
  row.a[1] =    0.0010238;
  row.a[2] =  -1.5228e-05;
  row.a[3] =   7.7636e-08;
  row.a[4] =  -1.1496e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2020
  row.a[0] =     -0.12946;
  row.a[1] =    0.0050677;
  row.a[2] =   -5.986e-05;
  row.a[3] =   2.5489e-07;
  row.a[4] =  -3.0749e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
