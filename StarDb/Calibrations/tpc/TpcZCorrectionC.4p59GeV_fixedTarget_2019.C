TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 4p59GeV_fixedTarget_2019
  row.a[0] =     0.044961;
  row.a[1] =  -0.00011182;
  row.a[2] =  -1.6343e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 4p59GeV_fixedTarget_2019
  row.a[0] =       0.2456;
  row.a[1] =   -0.0038997;
  row.a[2] =   5.6508e-05;
  row.a[3] =  -6.5148e-07;
  row.a[4] =   3.2094e-09;
  row.a[5] =  -5.5376e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 4p59GeV_fixedTarget_2019
  row.a[0] =    0.0050928;
  row.a[1] =   0.00019816;
  row.a[2] =  -1.3903e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 4p59GeV_fixedTarget_2019
  row.a[0] =     -0.05301;
  row.a[1] =    0.0047278;
  row.a[2] =  -1.8655e-05;
  row.a[3] =  -5.0553e-07;
  row.a[4] =   4.2471e-09;
  row.a[5] =  -9.1912e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
