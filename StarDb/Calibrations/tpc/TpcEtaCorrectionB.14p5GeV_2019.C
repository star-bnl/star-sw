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
  row.npar =            9;// 14p5GeV_2019
  row.a[0] =    -0.013871;
  row.a[1] =     0.026096;
  row.a[2] =      0.00418;
  row.a[3] =     -0.15501;
  row.a[4] =      0.31818;
  row.a[5] =       2.1443;
  row.a[6] =      -6.2179;
  row.a[7] =       5.5239;
  row.a[8] =      -1.6361;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.0;
  row.max =   2.0;
  row.npar =           10;// 14p5GeV_2019
  row.a[0] =    -0.016417;
  row.a[1] =     0.042226;
  row.a[2] =     -0.32129;
  row.a[3] =     0.033127;
  row.a[4] =      0.49555;
  row.a[5] =         0.26;
  row.a[6] =      -0.5331;
  row.a[7] =     -0.15474;
  row.a[8] =      0.30809;
  row.a[9] =    -0.078547;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   0.5;
  row.npar =            5;// 14p5GeV_2019
  row.a[0] =    -0.012116;
  row.a[1] =    -0.013438;
  row.a[2] =    -0.029231;
  row.a[3] =     -0.31046;
  row.a[4] =     -0.29401;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   1.0;
  row.npar =           10;// 14p5GeV_2019
  row.a[0] =    -0.023338;
  row.a[1] =    -0.045161;
  row.a[2] =     -0.31471;
  row.a[3] =     0.035269;
  row.a[4] =      0.55219;
  row.a[5] =     -0.41418;
  row.a[6] =     -0.67682;
  row.a[7] =      0.20311;
  row.a[8] =      0.38315;
  row.a[9] =     0.096383;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
