TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// tpcCorrection! Allocated rows: 48  Used rows: 48  Row size: 16 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[47]
// slope from Blair   1/( O2 in ppm., cm )  = -7.3603e-03;
// ===================================================================DriftCorr=
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcDriftDistOxygen",nrows);
  for (Int_t index = 1; index <=2; index++) {
    memset(&row,0,tableSet->GetRowSize()); 
    row.nrows      =  nrows;
    row.index      =  index;
    row.npar       =         2;  // Z3OGPHist834P04ifAuAu200
    row.a[0]	   =  3.57e-04;  // FitP->Draw("mu:y","chisq>0&&chisq<1000&&abs(mu)<.2&&x<14&&mu>.02","prof")
    row.a[1]	   = -7.00e-08;  // freeze Blair no. (Bliar gave %/meter=99.6-0.017502    O2 in ppm. V TPC obichno ~25 ppm) = 7e-8
    tableSet->AddAt(&row);
  }
#if 0
#if 0
  row.npar       =           2;       // Z3OGPHist833P04ifAuAu200  Z3OGPHist828P04ifAuAu200.root
  row.a[0]	 =  1.69134656265382925e-02;
  row.a[1]	 = -3.77753841206236094e-06;//1.80609e-04;
//   row.a[2]	 =-1.14232e-07;
//   row.a[3]	 = 3.55761e-11;
//   row.a[4]	 =-6.25535e-15;
//   row.a[5]	 = 6.37630e-19;
//   row.a[6]	 =-3.56367e-23;
//   row.a[7]	 = 8.60216e-28;
#endif
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar       =           2; // Z3OGPHist833P04ifAuAu200
  row.nrows      =  nrows;
  row.a[0]	 = 5.16727105618345756e-02; // fit in range [
  row.a[1]	 =-1.02848629375772658e-05;
#if 0
  row.npar       =           3;       //  Z3OGPHist833P04ifAuAu200  Z3OGPHist828P04ifAuAu200.root
  row.a[0]	 = 1.05651865055852126e-01;// 1.04586e-01;
  row.a[1]	 =-2.75631317131357867e-05;//-1.66577e-05;
  row.a[2]	 = 1.37707303991438017e-09;//-1.29942e-09;
    //  row.a[3]	 = 1.84944e-13;
#endif
  tableSet->AddAt(&row);
#endif
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
