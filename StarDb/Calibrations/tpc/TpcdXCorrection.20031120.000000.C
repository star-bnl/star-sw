TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows   = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      = nrows;       // Outer dXdECGFHist842P04ifAuAu200
  row.index      = 1;
  row.npar       = 4;
  row.min        = 0.95;
  row.max        = 2.20;
  row.a[0]	 = -4.61654e-01;//-4.61302e-01;// parameterization vs dX log2(dX)
  row.a[1]	 =  8.97964e-01;// 
  row.a[2]	 = -5.62668e-01;// 
  row.a[3]	 =  1.12212e-01;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      = nrows;       // Outer dXdECGFHist842P04ifAuAu200
  row.index      = 2;
  row.npar       = 6;
  row.min        = 0.2;
  row.max        = 1.4;
  row.a[0]	 = -1.55533e-01;// -1.65436e-01;//// parameterization vs dX log2(dX)
  row.a[1]	 =  1.29436e+00;// 
  row.a[2]	 = -3.62222e+00;// 
  row.a[3]	 =  3.98336e+00;// 
  row.a[4]	 = -1.44925e+00;// 
  row.a[5]	 =  9.42268e-03;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
