TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// scf_ctrl Allocated rows: 1  Used rows: 1  Row size: 8 bytes
//  Table: scf_ctrl_st[0]--> scf_ctrl_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_scf_ctrl")) return 0;
scf_ctrl_st row;
St_scf_ctrl *tableSet = new St_scf_ctrl("scf_ctrl",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.high_cut	 =          5; // high cut for central strip ;
    row.testTolerance	 =        0.2; // set to 20% ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
