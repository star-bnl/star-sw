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
  row.npar =            7;// 11p5GeV_2020
  row.a[0] =     0.011762;
  row.a[1] =    0.0016747;
  row.a[2] =  -4.9691e-05;
  row.a[3] =   6.1633e-07;
  row.a[4] =  -3.9591e-09;
  row.a[5] =   1.2804e-11;
  row.a[6] =  -1.6709e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 11p5GeV_2020
  row.a[0] =    -0.062402;
  row.a[1] =    0.0054991;
  row.a[2] =  -5.5225e-05;
  row.a[3] =  -1.7783e-07;
  row.a[4] =   4.6895e-09;
  row.a[5] =  -2.2494e-11;
  row.a[6] =   3.5027e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 11p5GeV_2020
  row.a[0] =     0.014668;
  row.a[1] =    0.0018882;
  row.a[2] =  -5.9272e-05;
  row.a[3] =   7.8236e-07;
  row.a[4] =  -5.4026e-09;
  row.a[5] =   1.8923e-11;
  row.a[6] =  -2.6743e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 11p5GeV_2020
  row.a[0] =    -0.056733;
  row.a[1] =    0.0064241;
  row.a[2] =  -7.9181e-05;
  row.a[3] =    1.168e-07;
  row.a[4] =   2.4155e-09;
  row.a[5] =   -1.294e-11;
  row.a[6] =   1.9175e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
