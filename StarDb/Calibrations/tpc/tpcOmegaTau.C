TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/tpc/.tpcOmegaTau/tpcOmegaTau Allocated rows: 1  Used rows: 1  Row size: 8 bytes
//  Table: tpcOmegaTau_st[0]--> tpcOmegaTau_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcOmegaTau")) return 0;
tpcOmegaTau_st row;
St_tpcOmegaTau *tableSet = new St_tpcOmegaTau("tpcOmegaTau",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.tensorV1	 =       1.35; // tensor for OmegaTau           ;
    row.tensorV2	 =        1.1; // tensor for OmegaTau    ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
