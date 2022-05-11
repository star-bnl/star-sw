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
  row.npar =            3;// 7p7GeV_2019
  row.a[0] =     0.008437;
  row.a[1] =  -3.6029e-05;
  row.a[2] =  -1.7027e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p7GeV_2019
  row.a[0] =     -0.02934;
  row.a[1] =    0.0028078;
  row.a[2] =  -5.3407e-05;
  row.a[3] =   3.9839e-07;
  row.a[4] =  -1.2574e-09;
  row.a[5] =   1.2906e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 7p7GeV_2019
  row.a[0] =     0.013383;
  row.a[1] =  -0.00010187;
  row.a[2] =   3.6084e-09;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p7GeV_2019
  row.a[0] =    -0.050117;
  row.a[1] =    0.0044321;
  row.a[2] =  -8.1608e-05;
  row.a[3] =   6.0094e-07;
  row.a[4] =  -1.9341e-09;
  row.a[5] =   2.1646e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
