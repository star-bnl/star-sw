TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// sce_ctrl Allocated rows: 1  Used rows: 1  Row size: 84 bytes
//  Table: sce_ctrl_st[0]--> sce_ctrl_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sce_ctrl")) return 0;
sce_ctrl_st row;
St_sce_ctrl *tableSet = new St_sce_ctrl("sce_ctrl",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.TrueClusterP	 =          1; // nbre of true cluster;
    row.GhostClusterP	 =          1; // nbre of ghost cluster;
    row.LostClusterP	 =          1; // nbre of lost cluster;
    row.TrueClusterN	 =          1; // nbre of true cluster;
    row.GhostClusterN	 =          1; // nbre of ghost cluster;
    row.LostClusterN	 =          1; // nbre of lost cluster;
    row.TrueSpt11	 =          1; // nbre of true space point from 1-1 case;
    row.GhostSpt11	 =          1; // nbre of ghost ;
    row.LostSpt11	 =          1; // nbre of lost   ;
    row.TrueSpt12	 =          1; // nbre of true space point from 1-2 case;
    row.GhostSpt12	 =          1; // nbre of ghost ;
    row.LostSpt12	 =          1; // nbre of lost   ;
    row.TrueSpt22	 =          1; // nbre of true space point from 2-2 case;
    row.GhostSpt22	 =          1; // nbre of ghost ;
    row.LostSpt22	 =          1; // nbre of lost   ;
    row.TrueSpt23	 =          1; // nbre of true space point from 2-3 case;
    row.GhostSpt23	 =          1; // nbre of ghost ;
    row.LostSpt23	 =          1; // nbre of lost   ;
    row.TrueSpt33	 =          1; // nbre of true space point from 3-3 case;
    row.GhostSpt33	 =          1; // nbre of ghost ;
    row.LostSpt33	 =          1; // nbre of lost   ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
