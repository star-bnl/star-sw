TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// tpcCorrection! Allocated rows: 48  Used rows: 48  Row size: 16 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[47]
// ===================================================================DriftCorr=
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcDriftDistOxygen",2);
memset(&row,0,tableSet->GetRowSize()); 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
 tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
