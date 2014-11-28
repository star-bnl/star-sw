St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/svgpars/shape Allocated rows: 2  Used rows: 2  Row size: 88 bytes
//  Table: svg_shape_st[0]--> svg_shape_st[1]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_svg_shape")) return 0;
svg_shape_st row;
St_svg_shape *tableSet = new St_svg_shape("shape",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.id	 =          1; // Type of SDD shape ;
    row.n_shape	 =          3; // number of shape parameters ;
    row.active[0]	 =          0; // active area of SDD wafers ;
    row.active[1]	 =          0;
    row.active[2]	 =          0;
    row.active[3]	 =          0;
    row.active[4]	 =          0;
    row.active[5]	 =          0;
    row.active[6]	 =          0;
    row.active[7]	 =          0;
    row.active[8]	 =          0;
    row.active[9]	 =          0;
    row.shape[0]	 =          3; // custom shape parameters of SDD wafers ;
    row.shape[1]	 =          3;
    row.shape[2]	 =      0.015;
    row.shape[3]	 =          0;
    row.shape[4]	 =          0;
    row.shape[5]	 =          0;
    row.shape[6]	 =          0;
    row.shape[7]	 =          0;
    row.shape[8]	 =          0;
    row.shape[9]	 =          0;
tableSet->AddAt(&row,0);
memset(&row,0,tableSet->GetRowSize());
    row.id	 =          2; // Type of SDD shape ;
    row.n_shape	 =          3; // number of shape parameters ;
    row.active[0]	 =          0; // active area of SSD wafers ;
    row.active[1]	 =          0;
    row.active[2]	 =          0;
    row.active[3]	 =          0;
    row.active[4]	 =          0;
    row.active[5]	 =          0;
    row.active[6]	 =          0;
    row.active[7]	 =          0;
    row.active[8]	 =          0;
    row.active[9]	 =          0;
    row.shape[0]	 =       3.75; // custom shape parameters of SSD wafer ;
    row.shape[1]	 =        2.1;
    row.shape[2]	 =      0.015;
    row.shape[3]	 =          0;
    row.shape[4]	 =          0;
    row.shape[5]	 =          0;
    row.shape[6]	 =          0;
    row.shape[7]	 =          0;
    row.shape[8]	 =          0;
    row.shape[9]	 =          0;
tableSet->AddAt(&row,1);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
