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
 row.a[0]	 =  6.43281e-02;//-1.83196e-02; // Outer
    row.a[1]	 =  3.10995e-04;
    row.a[2]	 = -7.82141e-07;
    row.a[3]	 =  0.0        ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
 row.a[0]	 =  4.15404e-02;//-4.11073e-02; // Inner
    row.a[1]	 =  8.59165e-04;
    row.a[2]	 = -2.67150e-06; 
    row.a[3]	 =  0.0;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
