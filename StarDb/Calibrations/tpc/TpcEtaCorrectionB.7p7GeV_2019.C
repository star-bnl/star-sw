TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.5;
  row.max =   1.4;
  row.npar =            5;// 7p7GeV_2019
  row.a[0] =   -0.0085686;
  row.a[1] =    0.0052493;
  row.a[2] =   -0.0077963;
  row.a[3] =      0.24813;
  row.a[4] =     -0.24471;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -0.9;
  row.max =   1.9;
  row.npar =           10;// 7p7GeV_2019
  row.a[0] =   -0.0065556;
  row.a[1] =     0.062506;
  row.a[2] =     -0.42225;
  row.a[3] =     -0.12461;
  row.a[4] =       1.0368;
  row.a[5] =      0.14201;
  row.a[6] =      -1.0621;
  row.a[7] =      0.27092;
  row.a[8] =      0.21824;
  row.a[9] =    -0.080775;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   0.5;
  row.npar =            7;// 7p7GeV_2019
  row.a[0] =    -0.008928;
  row.a[1] =   -0.0048477;
  row.a[2] =    -0.035835;
  row.a[3] =     -0.23514;
  row.a[4] =      0.10661;
  row.a[5] =      0.56217;
  row.a[6] =      0.25512;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   0.9;
  row.npar =           10;// 7p7GeV_2019
  row.a[0] =    -0.013934;
  row.a[1] =    -0.049902;
  row.a[2] =     -0.33778;
  row.a[3] =      0.11012;
  row.a[4] =      0.62755;
  row.a[5] =     -0.53235;
  row.a[6] =     -0.71433;
  row.a[7] =      0.43175;
  row.a[8] =      0.58583;
  row.a[9] =      0.14476;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
