St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/vertices/stk_vtx Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: stk_vtx_st[0]--> stk_vtx_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_stk_vtx")) return 0;
stk_vtx_st row;
St_stk_vtx *tableSet = new St_stk_vtx("stk_vtx",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.flag	 =          1; // Status flag. 1 means used space, no trac ;
    row.id	 =          1; // Identifier for Vertex row ;
    row.cov[0]	 =          0; // covariance matrix ;
    row.cov[1]	 =          0;
    row.cov[2]	 =          0;
    row.cov[3]	 =          0;
    row.cov[4]	 =          0;
    row.cov[5]	 =          0;
    row.t	 =          0; // Time of Vertex ;
    row.x[0]	 =          0; // Space point in STAR frame. ;
    row.x[1]	 =          0;
    row.x[2]	 =          0;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
