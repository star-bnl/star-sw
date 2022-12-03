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
  row.npar =            6;// fixedTarget_2020
  row.a[0] =     0.076006;
  row.a[1] =     -0.21673;
  row.a[2] =      -2.2195;
  row.a[3] =      -5.8401;
  row.a[4] =      -6.2099;
  row.a[5] =      -2.2124;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   0.0;
  row.npar =            7;// fixedTarget_2020
  row.a[0] =       0.1838;
  row.a[1] =     -0.78099;
  row.a[2] =      -4.1748;
  row.a[3] =      -8.2709;
  row.a[4] =      -8.0836;
  row.a[5] =      -3.7404;
  row.a[6] =     -0.65423;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.8;
  row.npar =            3;// fixedTarget_2020
  row.a[0] =      -1.3109;
  row.a[1] =      -1.8787;
  row.a[2] =      -0.6556;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.2;
  row.max =  -1.3;
  row.npar =            2;// fixedTarget_2020
  row.a[0] =     -0.54683;
  row.a[1] =     -0.30393;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
