TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// dbName/.data/StarDb/Calibrations/tpc/TpcDriftDistCorr Allocated rows: 2  Used rows: 2  Row size: 16 bytes
//  Table: TpcDriftDistCorr_st[0]--> TpcDriftDistCorr_st[1]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcDriftDistCorr")) return 0;
TpcDriftDistCorr_st row;
St_TpcDriftDistCorr *tableSet = new St_TpcDriftDistCorr("TpcDriftDistCorr",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.a[0]	 =  3.14074e-01;//-7.13934e-03; // Outer
    row.a[1]	 =  5.16564e-05;
    row.a[2]	 =  0;
    row.a[3]	 =  0;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.a[0]	 =  3.22604e-01;// 1.39031e-03; // Inner
    row.a[1]	 =  1.76342e-04;
    row.a[2]	 = -2.19382e-06;
    row.a[3]	 =  0;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
