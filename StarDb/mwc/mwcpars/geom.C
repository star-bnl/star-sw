St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/mwcpars/geom Allocated rows: 1  Used rows: 1  Row size: 28 bytes
//  Table: mwc_geo_st[0]--> mwc_geo_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_mwc_geo")) return 0;
mwc_geo_st row;
St_mwc_geo *tableSet = new St_mwc_geo("geom",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.init	 =          0; // Controls initialization ;
    row.neta	 =          4; // # sectors in eta ;
    row.nphi	 =         12; // #sector in phi ;
    row.r1max	 =    118.669; // Outer radius inner sectors ;
    row.r1min	 =     54.669; // Inner radius inner sectors ;
    row.r2max	 =    189.488; // Outer radius outer sectors ;
    row.r2min	 =    125.488; // Inner radius outer sectors ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
