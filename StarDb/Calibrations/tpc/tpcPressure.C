TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// tpcPressure Allocated rows: 1
//  Table: tpcPressure_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcPressure")) return 0;
  tpcPressure_st row;
  St_tpcPressure *tableSet = new St_tpcPressure("tpcPressure",1);
  //
  memset(&row,0,tableSet->GetRowSize());
//   row.A0 =   2.71602e+01;//   3.38503e-03 Hist247 09 Sep 02
//   row.A1 =  -3.94453e+00;//   4.88869e-04   -"- normalization for p>1.5GeV/c
//   row.A0 =   2.55857e+01;//  2.48851e-03 PressureCBHist251P02gh1 12 Sep 02
//   row.A1 =  -3.74027e+00;//  3.59424e-04   -"- normolization for 0.25 < p < 0.4 GeV/c
//   row.A0 =  2.53423e+01;//   3.03089e-03  PressureCBGPHist253P02gh1 13 Sep 02 shift wrt -8.22148e-01
//   row.A1 = -3.77898e+00;//   4.37728e-04    -"- normolization for 0.4 < p < 0.5 GeV/c
//  row.A0 =  2.79004e+01;//   1.26377e-02   PressureCBGPHist253P02gh1 13 Sep 02 shift wrt -8.22148e-01
//  row.A1 = -4.04000e+00;//   1.82385e-03     -"- normolization for 0.4 < p < 0.5 GeV/c
//   row.A0 =    2.71325e+01;// shift  7.49282e-02  PressureCBGPHist257P02gh1 16 Sep 02 shift wrt -8.22148e-01
//   row.A1 =   -3.91810e+00;//   1.82385e-03     -"- normolization for 0.4 < p < 0.5 GeV/c
//   row.A0 =    2.27091e+01;// PressureCGPHist284P02gh1  after non linear correction
//   row.A1 =   -3.26893e+00;// 
//   row.A0 =     1.99038e+01;// PressureGPHist291P02gh1 after  nonlinearity correction based on Positive tracks
//   row.A1 =    -2.86186e+00;// 05 Oct 02
  row.A0 =     1.99038e+01;// PressureGPHist291P02gh1 after  nonlinearity correction based on Positive tracks
  row.A1 =    -2.86186e+00;// 05 Oct 02
  tableSet->AddAt(&row);
// ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
