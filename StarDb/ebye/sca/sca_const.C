St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/sca/sca_const Allocated rows: 1  Used rows: 1  Row size: 112 bytes
//  Table: sca_const_st[0]--> sca_const_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sca_const")) return 0;
sca_const_st row;
St_sca_const *tableSet = new St_sca_const("sca_const",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.num_dim	 =          1; // ;
    row.minbinsize_x	 =      0.006; // ;
    row.maxbinsize_x	 =          2; // ;
    row.minbinsize_y	 =         10; // ;
    row.maxbinsize_y	 =         10; // ;
    row.step_binsize_x	 =        1.1; // ;
    row.step_binsize_y	 =          1; // ;
    row.auto_size	 =          0; // ;
    row.xmin	 =          0; // ;
    row.xmax	 =          1; // ;
    row.ymin	 =         -2; // ;
    row.ymax	 =          2; // ;
    row.zmin	 =          0; // ;
    row.zmax	 =        6.3; // ;
    row.num_ranks	 =          3; // ;
    row.rank[0]	 =          1; // ;
    row.rank[1]	 =          2;
    row.rank[2]	 =          3;
    row.rank[3]	 =          0;
    row.rank[4]	 =          0;
    row.rank[5]	 =          0;
    row.rank[6]	 =          0;
    row.rank[7]	 =          0;
    row.rank[8]	 =          0;
    row.rank[9]	 =          0;
    row.dithmin	 =         15; // ;
    row.dithscale	 =        150; // ;
    row.perfectdither	 =          0; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
