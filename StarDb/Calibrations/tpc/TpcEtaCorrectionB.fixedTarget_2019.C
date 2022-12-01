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
  row.npar =            4;// fixetTarget_2019
  row.a[0] =     0.014863;
  row.a[1] =    -0.072915;
  row.a[2] =    -0.025535;
  row.a[3] =      0.10258;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   0.0;
  row.npar =            3;// fixetTarget_2019
  row.a[0] =      0.22808;
  row.a[1] =      0.11823;
  row.a[2] =    -0.061055;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// fixetTarget_2019
  row.a[0] =      -1.1717;
  row.a[1] =      -1.7659;
  row.a[2] =     -0.65229;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            2;// fixetTarget_2019
  row.a[0] =     -0.64507;
  row.a[1] =     -0.36396;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
