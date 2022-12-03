TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.3;
  row.max =   1.3;
  row.npar =            5;// dAu200GeV_2021
  row.a[0] =     -0.01106;
  row.a[1] =     -0.06225;
  row.a[2] =      0.28664;
  row.a[3] =     0.094358;
  row.a[4] =     -0.37338;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -0.6;
  row.max =   1.8;
  row.npar =            9;// dAu200GeV_2021
  row.a[0] =        0.023;
  row.a[1] =     0.028676;
  row.a[2] =     -0.23145;
  row.a[3] =     -0.74753;
  row.a[4] =       1.4467;
  row.a[5] =       1.3668;
  row.a[6] =      -3.6756;
  row.a[7] =       2.2521;
  row.a[8] =     -0.44338;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.3;
  row.max =   0.3;
  row.npar =            5;// dAu200GeV_2021
  row.a[0] =    -0.010113;
  row.a[1] =      0.02621;
  row.a[2] =      0.13478;
  row.a[3] =     -0.23483;
  row.a[4] =     -0.37759;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   0.5;
  row.npar =            7;// dAu200GeV_2021
  row.a[0] =     0.021287;
  row.a[1] =     0.034003;
  row.a[2] =      -0.3398;
  row.a[3] =    -0.056735;
  row.a[4] =       1.1738;
  row.a[5] =       1.1792;
  row.a[6] =      0.31586;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
