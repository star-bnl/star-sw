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
    row.a[0]	 =   0.279582; // ;
    row.a[1]	 = 0.000839322;
    row.a[2]	 = 1.76737e-06;
    row.a[3]	 = -2.69601e-08;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.a[0]	 =   0.308298; // ;
    row.a[1]	 = 8.27434e-05;
    row.a[2]	 = 2.74249e-06;
    row.a[3]	 = -1.2062e-08;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
