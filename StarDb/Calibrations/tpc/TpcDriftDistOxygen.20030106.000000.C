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
  row.npar       =           2;//   Outer Z3OCGPHist535P03if.root
  row.a[0]	 =-1.00203e-02;//
  row.a[1]	 = 7.69039e-07;//1.7502e-6 slope from Blair   1/( O2 in ppm., cm ) 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar        =          2; //  Inner Z3OCGPHist535P03if
  row.a[0]	  =-1.97615e-02;//-7.3603e-03;//  4.20540160413917147e+03;// Inner
  row.a[1]	  = 1.27510e-06;// 1.7502e-06;// slope from Blair   1/( O2 in ppm., cm ) 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
