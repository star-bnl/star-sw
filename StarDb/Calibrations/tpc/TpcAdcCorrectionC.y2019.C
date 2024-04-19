#include "tables/St_tpcCorrection_Table.h"

TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.TpcAdcCorrectionC/TpcAdcCorrectionC Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrectionC",nrows);
  memset(&row,0,tableSet->GetRowSize());
  /*
    ~/work/Tpc/TpcRS/2019/TpcRS_ped/SparseFit $ root.exe SparseGP.root
    FitP->Draw("mu+3.33398e-01+1.37760e-01:z3>>O","j==2&&prob>0.01&&dmu<0.01&&dsigma<0.01","profs")
    O->Fit("pol4","er","",4,10)
   */
  row.type     =           10; //
  row.idx      =            1; //
  row.nrows    =        nrows; //
  row.npar     =            5; //
  row.a[0]     =  4.73502e+00; //
  row.a[1]     = -2.05210e+00; //
  row.a[2]     =  4.25137e-01; //
  row.a[3]     = -4.00479e-02; //
  row.a[4]     =  1.40733e-03; //
  tableSet->AddAt(&row);     // 
  memset(&row,0,tableSet->GetRowSize());
  /* 
     FitP->Draw("mu+4.68447e-02 +1.26432e-01:z3>>I","j==1&&prob>0.01&&dmu<0.01&&dsigma<0.01","profs")
     I->Fit("pol4","er","",4,10)
   */
  row.type     =           10; //
  row.idx      =            2; //
  row.nrows    =        nrows; //
  row.npar     =            5; //
  row.a[0]     =  3.78959e+00; //
  row.a[1]     = -1.53943e+00; //
  row.a[2]     =  2.92137e-01; //
  row.a[3]     = -2.58066e-02; //
  row.a[4]     =  8.61995e-04; //
  tableSet->AddAt(&row);     // 
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
