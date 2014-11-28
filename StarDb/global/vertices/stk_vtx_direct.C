St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/vertices/stk_vtx_direct Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: stk_vtx_direct_st[0]--> stk_vtx_direct_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_stk_vtx_direct")) return 0;
stk_vtx_direct_st row;
St_stk_vtx_direct *tableSet = new St_stk_vtx_direct("stk_vtx_direct",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.init_ran	 = 1081518272; // if not 0 init seed with this each event ;
    row.sx[0]	 =     0.0005; // sigma for smearing ;
    row.sx[1]	 =     0.0005;
    row.sx[2]	 =     0.0005;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
