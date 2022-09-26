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
  row.npar =            3;// COLGeV_2019
  row.a[0] =    -0.042759;
  row.a[1] =   0.00064036;
  row.a[2] =  -1.8492e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2019
  row.a[0] =    -0.058497;
  row.a[1] =    0.0047124;
  row.a[2] =  -7.2467e-05;
  row.a[3] =   4.3844e-07;
  row.a[4] =  -9.0519e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// COLGeV_2019
  row.a[0] =    -0.041137;
  row.a[1] =   0.00063734;
  row.a[2] =  -1.9373e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2019
  row.a[0] =    -0.047852;
  row.a[1] =    0.0050887;
  row.a[2] =  -7.9534e-05;
  row.a[3] =   4.7528e-07;
  row.a[4] =  -9.6651e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
