St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ffspars/fspar Allocated rows: 1  Used rows: 1  Row size: 24 bytes
//  Table: ffs_fspar_st[0]--> ffs_fspar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ffs_fspar")) return 0;
ffs_fspar_st row;
St_ffs_fspar *tableSet = new St_ffs_fspar("fspar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.padwid[0]	 =      0.335; // pad width (cm) ;
    row.padwid[1]	 =          3;
    row.sprf_0[0]	 =        300; // prf intrinsic term ;
    row.sprf_0[1]	 =        300;
    row.sprf_ta[0]	 =          0; // prf tan-alpha term ;
    row.sprf_ta[1]	 =         -1;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
