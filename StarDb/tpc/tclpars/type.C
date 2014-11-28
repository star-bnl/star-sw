St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tclpars/type Allocated rows: 1  Used rows: 1  Row size: 28 bytes
//  Table: tcl_tpc_index_type_st[0]--> tcl_tpc_index_type_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tcl_tpc_index_type")) return 0;
tcl_tpc_index_type_st row;
St_tcl_tpc_index_type *tableSet = new St_tcl_tpc_index_type("type",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.tphit_mhitstpc	 =          1; // correlate tphit and mhits_tpc tables ;
    row.tphit_tpcluster	 =          2; // correlate tphit and tpcluster tables ;
    row.tphit_tpsegment	 =          3; // correlate tphit and tpsegment tables ;
    row.tphit_tptrack	 =          4; // correlate tphit and tptrack tables ;
    row.tpsegment_mctrack	 =          5; // correlate tpsegment and mctrack tables ;
    row.tptrack_mctrack	 =          6; // correlate tptrack and mctrack tables ;
    row.tptrack_tpsegment	 =          7; // correlate tptrack and tpsegment tables ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
