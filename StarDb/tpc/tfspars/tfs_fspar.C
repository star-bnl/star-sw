St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tfspars/tfs_fspar Allocated rows: 1  Used rows: 1  Row size: 160 bytes
//  Table: tfs_fspar_st[0]--> tfs_fspar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tfs_fspar")) return 0;
tfs_fspar_st row;
St_tfs_fspar *tableSet = new St_tfs_fspar("tfs_fspar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.irmax	 =         13; // number of inner sector rows ;
    row.nrow	 =         45; // total number of TPC rows ;
    row.nsect	 =         24; // total number of TPC sectors ;
    row.padlen[0]	 =        1.2; // pad length (cm) ;
    row.padlen[1]	 =          2;
    row.padsep[0]	 =        4.8; // pad pitch (cm) ;
    row.padsep[1]	 =          2;
    row.padwid[0]	 =      0.355; // pad width (cm) ;
    row.padwid[1]	 =       0.67;
    row.sldiff	 =      0.035; // longitudinal diffusion ;
    row.tshape	 =        180; // shaping time (ns) ;
    row.zbmax	 =        512; // total time bins ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
