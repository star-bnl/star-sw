TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row; // parameterization of ped RMS versus x = pad/NoOfPads - 0.5
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcPadPedRMS",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx      = 1;
  row.nrows    = nrows;
  row.min      = -0.5;
  row.max      =  0.5;
  row.npar     =    3; // /net/l404/data/fisyak/Tpc/ped/tpx_23074031
  row.a[0]     = 6.25337e-01;
  row.a[1]     = 0.00000e+00;
  row.a[2]     = 5.54285e-01;
  row.a[3]     = 4.81557e+01; // <ped>
  row.a[4]     = 8.99656e+00; // RMSped
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx      = 2;
  row.nrows    = nrows; 
  row.min      = -0.5;
  row.max      =  0.5;
  row.npar     =    3; // /net/l404/data/fisyak/Tpc/ped/tpc_23074031
  row.a[0]     = 8.26510e-01;
  row.a[1]     = 0.00000e+00;
  row.a[2]     = 3.81718e-01;
  row.a[3]     = 7.89299e+01; // 8.61555e+01;  // <ped>
  row.a[4]     = 9.43011e+00; // 8.91453e+00;  // RMSped
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
