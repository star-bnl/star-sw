TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows  = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",nrows);
  for (Int_t indx = 1; indx <=2; indx++) {
    memset(&row,0,tableSet->GetRowSize()); 
    row.nrows     = nrows;
    row.npar      =           2;       // PressureGFHist840P04ifAuAu200
    row.a[0]	  = 2.14010938228920971e+01;// pol1->GetParameter(0)-f0_0->GetFunction("GF")->GetParameter(1)
    row.a[1]	  =-3.09091839699658433e+00;// pol1->GetParameter(1)
#if 0
    row.npar      =           2;       // PressureGPHist827P04ifAuAu200
    row.a[0]	  = 2.06043113506573725e+01;
    row.a[1]	  =-2.97679307961439399e+00; 
#if 0
    row.npar      =           2;       // PressureGPHist827P04ifAuAu200
    row.a[0]	  = 1.98889e+01;
    row.a[1]	  =-2.87419e+00;
#endif
#endif
    row.index     = indx;
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
