St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/sgrpars/pix_info Allocated rows: 4  Used rows: 4  Row size: 28 bytes
//  Table: sgr_pixmap_st[0]--> sgr_pixmap_st[3]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sgr_pixmap")) return 0;
sgr_pixmap_st row;
St_sgr_pixmap *tableSet = new St_sgr_pixmap("pix_info",4);
//
memset(&row,0,tableSet->GetRowSize());
    row.NHitsMax	 =         10; // Maximum number of hits per pixel. ;
    row.NPhi	 =        576; // Number of bins in phi. ;
    row.NZ	 =       1600; // Number of bins in Zscaled. ;
    row.PhiMin	 =          0; // Minimum Phi values in pixmap. ;
    row.PhiMax	 =        360; // Maximum Phi values in pixmap. ;
    row.ZMin	 =        -20; // Minimum Zscaled values in pixmap. ;
    row.ZMax	 =         20; // Maximum Zscaled values in pixmap. ;
tableSet->AddAt(&row,0);
memset(&row,0,tableSet->GetRowSize());
    row.NHitsMax	 =         10; // Maximum number of hits per pixel. ;
    row.NPhi	 =        284; // Number of bins in phi. ;
    row.NZ	 =        800; // Number of bins in Zscaled. ;
    row.PhiMin	 =          0; // Minimum Phi values in pixmap. ;
    row.PhiMax	 =        360; // Maximum Phi values in pixmap. ;
    row.ZMin	 =        -20; // Minimum Zscaled values in pixmap. ;
    row.ZMax	 =         20; // Maximum Zscaled values in pixmap. ;
tableSet->AddAt(&row,1);
memset(&row,0,tableSet->GetRowSize());
    row.NHitsMax	 =         10; // Maximum number of hits per pixel. ;
    row.NPhi	 =        142; // Number of bins in phi. ;
    row.NZ	 =        400; // Number of bins in Zscaled. ;
    row.PhiMin	 =          0; // Minimum Phi values in pixmap. ;
    row.PhiMax	 =        360; // Maximum Phi values in pixmap. ;
    row.ZMin	 =        -20; // Minimum Zscaled values in pixmap. ;
    row.ZMax	 =         20; // Maximum Zscaled values in pixmap. ;
tableSet->AddAt(&row,2);
memset(&row,0,tableSet->GetRowSize());
    row.NHitsMax	 =         10; // Maximum number of hits per pixel. ;
    row.NPhi	 =         72; // Number of bins in phi. ;
    row.NZ	 =        200; // Number of bins in Zscaled. ;
    row.PhiMin	 =          0; // Minimum Phi values in pixmap. ;
    row.PhiMax	 =        360; // Maximum Phi values in pixmap. ;
    row.ZMin	 =        -20; // Minimum Zscaled values in pixmap. ;
    row.ZMax	 =         20; // Maximum Zscaled values in pixmap. ;
tableSet->AddAt(&row,3);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
