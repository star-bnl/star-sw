TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// TpcDriftDistCorr Allocated rows: 48  Used rows: 48  Row size: 16 bytes
//  Table: TpcDriftDistCorr_st[0]--> TpcDriftDistCorr_st[47]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcDriftDistCorr")) return 0;
TpcDriftDistCorr_st row;
St_TpcDriftDistCorr *tableSet = new St_TpcDriftDistCorr("TpcDriftDistCorr",48);
//
memset(&row,0,tableSet->GetRowSize());
 row.a[0]	 =  0.0; // Outer
    row.a[1]	 =  0.0;
    row.a[2]	 =  0.0;
    row.a[3]	 =  0.0;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.a[0]	 =    0.0;// Inner
    row.a[1]	 =    0.0;
    row.a[2]	 =    0.0;
    row.a[3]	 =    0.0;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
