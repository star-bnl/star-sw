TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =  -0.1;
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =     0.010206;
  row.a[1] =     -0.18422;
  row.a[2] =     -0.24464;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            5;// 13p5GeV_fixedTarget_2020
  row.a[0] =       0.2321;
  row.a[1] =      0.30524;
  row.a[2] =      0.52267;
  row.a[3] =      0.55322;
  row.a[4] =      0.16257;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 13p5GeV_fixedTarget_2020
  row.a[0] =      0.97994;
  row.a[1] =       3.2606;
  row.a[2] =       3.1626;
  row.a[3] =      0.93993;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            2;// 13p5GeV_fixedTarget_2020
  row.a[0] =     -0.57807;
  row.a[1] =     -0.31387;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
