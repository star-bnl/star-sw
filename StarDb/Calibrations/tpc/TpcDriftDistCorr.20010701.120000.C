TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// TpcDriftDistCorr Allocated rows: 48  Used rows: 48  Row size: 16 bytes
//  Table: TpcDriftDistCorr_st[0]--> TpcDriftDistCorr_st[47]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcDriftDistCorr")) return 0;
  TpcDriftDistCorr_st row;
  St_TpcDriftDistCorr *tableSet = new St_TpcDriftDistCorr("TpcDriftDistCorr",2);
//
  memset(&row,0,tableSet->GetRowSize());
  row.a[0]	 =    2.79582e-01;//-4.75612e-02; // Outer
  row.a[1]	 =    8.39322e-04;
  row.a[2]	 =    1.76737e-06;
  row.a[3]	 =   -2.69601e-08;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.a[0]	 =    3.08298e-01;//-1.88454e-02;// Inner
  row.a[1]	 =    8.27434e-05;
  row.a[2]	 =    2.74249e-06;
  row.a[3]	 =   -1.20620e-08;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
