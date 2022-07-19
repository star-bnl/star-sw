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
  row.npar =            2;// COLGeV_2019
  row.a[0] =    0.0031297;
  row.a[1] =   5.0799e-05;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2019
  row.a[0] =     -0.12167;
  row.a[1] =    0.0071847;
  row.a[2] =  -0.00010873;
  row.a[3] =   6.1481e-07;
  row.a[4] =  -1.1775e-09;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            2;// COLGeV_2019
  row.a[0] =    0.0068028;
  row.a[1] =    2.253e-05;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2019
  row.a[0] =     -0.13204;
  row.a[1] =    0.0087461;
  row.a[2] =  -0.00013491;
  row.a[3] =   7.6431e-07;
  row.a[4] =  -1.4661e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
