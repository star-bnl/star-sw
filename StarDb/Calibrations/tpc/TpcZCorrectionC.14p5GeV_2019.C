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
  row.npar =            3;// 14p5GeV_2019
  row.a[0] =    0.0077707;
  row.a[1] =   5.5066e-07;
  row.a[2] =  -4.2442e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 14p5GeV_2019
  row.a[0] =   -0.0072647;
  row.a[1] =    0.0021176;
  row.a[2] =  -4.0803e-05;
  row.a[3] =   2.8166e-07;
  row.a[4] =  -7.8101e-10;
  row.a[5] =   5.9026e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 14p5GeV_2019
  row.a[0] =    0.0099418;
  row.a[1] =  -2.4513e-05;
  row.a[2] =  -3.7793e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 14p5GeV_2019
  row.a[0] =    -0.027434;
  row.a[1] =    0.0036519;
  row.a[2] =  -6.7165e-05;
  row.a[3] =   4.6757e-07;
  row.a[4] =  -1.3954e-09;
  row.a[5] =   1.3954e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
