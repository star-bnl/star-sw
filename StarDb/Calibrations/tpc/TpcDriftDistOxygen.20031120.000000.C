TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// tpcCorrection! Allocated rows: 48  Used rows: 48  Row size: 16 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[47]
// ===================================================================DriftCorr=xs
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcDriftDistOxygen",2);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =           4;// Z3OGPHist719P04id  2;//   Outer  Z3OGPHist718Fast
  row.a[0]	 = 4.16819e-02;//                    7.99302e-02;//
  row.a[1]	 = 2.03867e-05;//                   -8.76725e-06;//1.7502e-6 slope from Blair   1/( O2 in ppm., cm ) 
  row.a[2]	 =-6.02006e-09;// 
  row.a[3]	 = 3.86332e-13;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar        =           5;//Z3OGPHist719P04id            2;//  Inner Z3OGPHist718Fast
  row.a[0]	  = 3.93607e-02;//                   3.54729e-02;// 
  row.a[1]	  = 1.00883e-05;//                  -3.63174e-06;// 
  row.a[2]	  =-7.31306e-09;//
  row.a[3]	  = 1.24258e-12;//
  row.a[4]	  =-6.67756e-17;//
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
